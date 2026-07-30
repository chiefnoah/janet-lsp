(import ./index)
(import ./platform)
(import ./uri)
(import spork/path)

(def format-version 6)
(def cache-magic "janet-lsp-cache\0")

(defn- ensure-directory [directory]
  (unless (os/stat directory)
    (def parent (path/parent directory))
    (when (and (not (empty? parent)) (not= parent directory))
      (ensure-directory parent))
    (os/mkdir directory))
  directory)

(defn directory []
  (or (os/getenv "JANET_LSP_CACHE_DIR")
      (when-let [xdg (os/getenv "XDG_CACHE_HOME")]
        (path/join xdg "janet-lsp"))
      (when-let [home (os/getenv "HOME")]
        (path/join home ".cache" "janet-lsp"))
      (path/join (platform/temp-directory) "janet-lsp-cache")))

(defn path-for [root-uri]
  (path/join (directory) (string "workspace-" (index/content-hash root-uri) ".jdn")))

(defn- metadata [filepath]
  (when-let [stat (os/stat filepath)]
    {:size (stat :size)
     :modified (stat :modified)
     :inode (stat :inode)
     :device (stat :dev)}))

(defn- sample [filepath]
  (try
    (let [before (metadata filepath)
          content (and before (slurp filepath))
          after (metadata filepath)]
      (when (and content (deep= before after))
        {:metadata after :content-hash (index/content-hash content)}))
    ([_] nil)))

(defn- nonnegative-integer? [value]
  (and (number? value) (= value (math/floor value)) (>= value 0)))

(defn- position? [position]
  (and (dictionary? position)
       (nonnegative-integer? (position :line))
       (nonnegative-integer? (position :character))))

(defn- range? [range]
  (and (dictionary? range)
       (position? (range :start))
       (position? (range :end))))

(defn- definition? [definition document-uri]
  (and (dictionary? definition)
       (= document-uri (definition :uri))
       (string? (definition :name))
       (number? (definition :kind))
       (string? (definition :form))
       (has-value? [true false] (definition :private))
       (range? (definition :range))
       (range? (definition :selection-range))
       (or (nil? (definition :type-target))
           (and (dictionary? (definition :type-target))
                (string? (get-in definition [:type-target :name]))
                (range? (get-in definition [:type-target :range]))))
       (or (nil? (definition :return-target))
           (string? (definition :return-target)))
       (indexed? (definition :implementation-targets))
       (all |(and (dictionary? $) (string? ($ :name)) (range? ($ :range)))
            (definition :implementation-targets))
       (indexed? (definition :children))
       (all |(and (dictionary? $)
                  (string? ($ :name))
                  (number? ($ :kind))
                  (range? ($ :range))
                  (range? ($ :selection-range)))
            (definition :children))))

(defn- reference? [reference document-uri]
  (and (dictionary? reference)
       (= document-uri (reference :uri))
       (string? (reference :name))
       (range? (reference :range))))

(defn- import? [imported]
  (and (dictionary? imported)
       (string? (imported :module))
       (string? (imported :kind))
       (string? (imported :alias))
       (string? (imported :prefix))
       (indexed? (imported :only))
       (all string? (imported :only))
       (has-value? [true false] (imported :top-level))
       (range? (imported :range))))

(defn- callable? [callable document-uri]
  (and (dictionary? callable)
       (= document-uri (callable :uri))
       (string? (callable :name))
       (string? (callable :identity))
       (number? (callable :kind))
       (string? (callable :form))
       (has-value? [true false] (callable :local))
       (range? (callable :range))
       (range? (callable :selection-range))
       (or (nil? (callable :scope-range)) (range? (callable :scope-range)))))

(defn- call? [call document-uri]
  (and (dictionary? call)
       (= document-uri (call :uri))
       (string? (call :name))
       (or (nil? (call :caller)) (string? (call :caller)))
       (or (nil? (call :identity)) (string? (call :identity)))
       (range? (call :range))))

(defn- record? [record document-uri]
  (and (dictionary? record)
       (= document-uri (record :uri))
       (string? (record :content-hash))
       (indexed? (record :definitions))
       (indexed? (record :references))
       (indexed? (record :imports))
       (indexed? (record :callables))
       (indexed? (record :calls))
       (all |(definition? $ document-uri) (record :definitions))
       (all |(reference? $ document-uri) (record :references))
       (all import? (record :imports))
       (all |(callable? $ document-uri) (record :callables))
       (all |(call? $ document-uri) (record :calls))))

(defn- envelope? [envelope root-uri exclusions]
  (and (dictionary? envelope)
       (= format-version (envelope :version))
       (= root-uri (envelope :root-uri))
       (deep= (array ;exclusions) (array ;(envelope :exclusions)))
       (dictionary? (envelope :entries))))

(defn- read-envelope [cache-path root-uri exclusions]
  (when (os/stat cache-path)
    (try
      (let [content (slurp cache-path)
            parsed (and (string/has-prefix? cache-magic content)
                        (unmarshal (string/slice content (length cache-magic))))]
        (and (envelope? parsed root-uri exclusions) parsed))
      ([_] nil))))

(defn- disk-copy [disk-index]
  (def copy @{})
  (eachp [document-uri record] disk-index
    (put copy document-uri
         (merge record
                {:references
                 (map |(if (= :import ($ :identity-kind))
                         (merge $ {:identity nil :identity-kind nil}) $)
                      (record :references))
                 # Call targets are reconstructed from references and imports.
                 :calls (map |(merge $ {:identity nil}) (record :calls))})))
  copy)

(defn load [cache-path root-uri root exclusions]
  (var envelope (read-envelope cache-path root-uri exclusions))
  (def cache-metadata (metadata cache-path))
  (when (and (os/stat cache-path) (not envelope))
    (try (os/rm cache-path) ([_] nil)))
  (def filepaths (index/files root exclusions))
  (var valid @{})
  (var dirty @[])
  (var hashed 0)
  (each filepath filepaths
    (def document-uri (uri/path->file-uri filepath))
    (def entry (and envelope (get-in envelope [:entries document-uri])))
    (def current-metadata (metadata filepath))
    # Integer-second mtimes are ambiguous only when the cache was written in the
    # same timestamp tick as its source. Older unchanged files need no reread.
    (def metadata-current
      (and current-metadata
           (deep= current-metadata (and entry (entry :metadata)))
           cache-metadata
           (> (cache-metadata :modified) (current-metadata :modified))))
    (def current
      (when (and (dictionary? entry) (not metadata-current))
        (+= hashed 1)
        (sample filepath)))
    (if (and (dictionary? entry)
             (or metadata-current
                 (and current
                      (deep= current
                             {:metadata (entry :metadata)
                              :content-hash (entry :content-hash)})))
             (= (entry :content-hash) (get-in entry [:record :content-hash]))
             (record? (entry :record) document-uri))
      (put valid document-uri (entry :record))
      (array/push dirty filepath)))
  {:index valid
   :dirty dirty
   :files filepaths
   :hashed hashed
   :complete (and envelope
                  (empty? dirty)
                  (= (length filepaths) (length (envelope :entries))))})

(defn write [cache-path root-uri exclusions disk-index]
  (def entries @{})
  (def copy (disk-copy disk-index))
  (eachp [document-uri record] copy
    (when-let [filepath (uri/file-uri->path document-uri)
               found (sample filepath)]
      (when (= (record :content-hash) (found :content-hash))
        (put entries document-uri {:metadata (found :metadata)
                                   :content-hash (found :content-hash)
                                   :record record}))))
  (def envelope {:version format-version
                 :root-uri root-uri
                 :exclusions (array ;exclusions)
                 :entries entries})
  (def root (uri/file-uri->path root-uri))
  (def complete
    (and (= (length entries) (length copy))
         root
         (= (length entries) (length (index/files root exclusions)))))
  (ensure-directory (path/dirname cache-path))
  (def temporary (string cache-path "." (os/getpid) ".tmp"))
  (try
    (do
      (spit temporary (string cache-magic (marshal envelope)))
      (os/rename temporary cache-path)
      complete)
    ([err]
      (when (os/stat temporary) (try (os/rm temporary) ([_] nil)))
      (error err))))

(defn rebuild [cache-path root-uri root exclusions &opt persist]
  (default persist true)
  (def loaded (load cache-path root-uri root exclusions))
  (def records (loaded :index))
  (each filepath (loaded :dirty)
    (try
      (let [document-uri (uri/path->file-uri filepath)]
        (put records document-uri (index/analyze document-uri (slurp filepath))))
      ([_] nil)))
  (index/relink @{:index records})
  (when persist
    (try (write cache-path root-uri exclusions records) ([_] nil)))
  records)

(defn rebuild-dirty [cache-path root-uri root exclusions &opt persist]
  (def loaded (load cache-path root-uri root exclusions))
  (def changes @{})
  (each filepath (loaded :dirty)
    (try
      (let [document-uri (uri/path->file-uri filepath)]
        (put changes document-uri (index/analyze document-uri (slurp filepath))))
      ([_] nil)))
  (def cached
    (when persist
      (def records (loaded :index))
      (eachp [document-uri record] changes (put records document-uri record))
      (write cache-path root-uri exclusions records)))
  {:changes changes :cached cached})

(defn rebuild-changes [cache-path root-uri root exclusions]
  ((rebuild-dirty cache-path root-uri root exclusions) :changes))
