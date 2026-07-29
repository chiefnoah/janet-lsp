(import ./index)
(import ./document-features)
(import ./lookup)
(import ./parser)
(import ./position)
(import ./server-utils)
(import ./uri)
(import spork/path)

(defn symbol-context [state params]
  (def document-uri (server-utils/document-uri params))
  (def document (server-utils/document state params))
  (def content (document :content))
  (def location (server-utils/request-byte-position state params content))
  (def word (lookup/word-at location content))
  (def name (word :word))
  (def workspace (server-utils/document-workspace state document))
  (def local-declaration
    (parser/binding-at location content (get-in document [:analysis :syntax-tree])))
  (def parser-resolved
    (and (not (empty? name))
         (not local-declaration)
         (parser/binding-definition-at location content name)))
  (def parser-resolved-binding
    (and parser-resolved
         (parser/binding-at (get-in parser-resolved [:range :start]) content
                            (get-in document [:analysis :syntax-tree]))))
  (def resolved
    (and (not (empty? name))
         (if local-declaration
           {:name name :range local-declaration}
           (and parser-resolved-binding
                {:name name :range parser-resolved-binding}))))
  (var indexed (and (not (empty? name))
                    (index/resolve-definition workspace document-uri name location)))
  (def resolved-indexed?
    (and resolved indexed
         (server-utils/same-position?
           (get-in resolved [:range :start])
           (get-in indexed [:selection-range :start]))))
  (def occurrence
    (first
      (filter |(and (server-utils/same-position?
                      (get-in $ [:range :start])
                      {:line (location :line)
                       :character (first (word :range))})
                    (= name ($ :name)))
              (get-in document [:analysis :index :references] @[]))))
  (when (and indexed
             (not (or (and occurrence
                           (= (occurrence :identity) (indexed :identity)))
                      (server-utils/same-position?
                        (get-in indexed [:selection-range :start])
                        {:line (location :line)
                         :character (first (word :range))}))))
    (set indexed nil))
  (def local (and resolved (not resolved-indexed?) resolved))
  {:uri document-uri
   :document document
   :content content
   :location location
   :word word
   :name name
   :occurrence occurrence
   :workspace workspace
   :local local
   :indexed (and (not local) indexed)})

(defn on-definition [state params]
  (def context (symbol-context state params))
  (def name (context :name))
  (cond
    (context :local)
    [:ok state {:uri (context :uri)
                :range (server-utils/lsp-range state (context :content)
                                               (get-in context [:local :range]))}]

    (context :indexed)
    (let [definition (context :indexed)
          target-uri (definition :uri)
          target-content (server-utils/content state target-uri)]
      [:ok state
       (if target-content
         {:uri target-uri
          :range (server-utils/lsp-range state target-content (definition :range))}
         :null)])

    (and (not (empty? name)) (context :occurrence))
    (if-let [binding (get-in context [:document :eval-env (symbol name)])
             [source-path source-line source-column] (binding :source-map)
             target-path (path/abspath source-path)
             found (os/stat target-path)
             target-content (slurp target-path)
             target-position
             (position/byte->lsp-position
               target-content
               {:line (max 0 (dec source-line))
                :character (max 0 (dec source-column))}
               (state :position-encoding))]
      [:ok state {:uri (uri/path->file-uri target-path)
                  :range {:start target-position :end target-position}}]
      [:ok state :null])

    [:ok state :null]))

(defn- definition-location [state definition]
  (when-let [content (server-utils/content state (definition :uri))]
    {:uri (definition :uri)
     :range (server-utils/lsp-range state content (definition :range))}))

(defn on-type-definition [state params]
  (def context (symbol-context state params))
  (if-let [identity (get-in context [:indexed :identity])
           definition (index/type-definition (context :workspace) identity)
           location (definition-location state definition)]
    [:ok state location]
    [:ok state :null]))

(defn on-implementation [state params]
  (def context (symbol-context state params))
  (if-let [identity (get-in context [:indexed :identity])]
    [:ok state
     (catseq [definition :in (index/implementations (context :workspace) identity)
              :let [location (definition-location state definition)]
              :when location]
       location)]
    [:ok state :null]))

(defn- document-symbol [state content definition definitions]
  {:name (definition :name)
   :kind (definition :kind)
   :range (server-utils/lsp-range state content (definition :range))
   :selectionRange (server-utils/lsp-range state content
                                            (definition :selection-range))
   :children
   (array
     ;(map |{:name ($ :name)
             :kind ($ :kind)
             :range (server-utils/lsp-range state content ($ :range))
             :selectionRange (server-utils/lsp-range
                               state content ($ :selection-range))}
           (definition :children))
     ;(map |(document-symbol state content $ definitions)
           (filter |(= (definition :identity) ($ :container)) definitions)))})

(defn on-document-symbols [state params]
  (if-let [document (server-utils/document state params)]
    (let [record (get-in document [:analysis :index])
          definitions (record :definitions)]
      [:ok state
       (map |(document-symbol state (document :content) $ definitions)
            (filter |(nil? ($ :container)) definitions))])
    [:ok state @[]]))

(defn on-workspace-symbols [state params]
  (def query (string/ascii-lower (or (get params "query") "")))
  (def symbols @[])
  (each workspace (values (state :workspaces))
    (each definition (index/definitions workspace)
      (when (string/find query (string/ascii-lower (definition :name)))
        (when-let [content (server-utils/content state (definition :uri))]
          (array/push symbols
                      {:name (definition :name)
                       :kind (definition :kind)
                       :location {:uri (definition :uri)
                                  :range (server-utils/lsp-range
                                           state content
                                           (definition :selection-range))}})))))
  [:ok state symbols])

(defn- raw-references [context include-declaration]
  (var locations @[])
  (if (context :local)
    (let [definition-start (get-in context [:local :range :start])
          record (get-in context [:document :analysis :index])
          indexed-definition
          (first (filter |(server-utils/same-position?
                            definition-start (get-in $ [:selection-range :start]))
                         (record :definitions)))
          identity (if indexed-definition
                     (indexed-definition :identity)
                     (string (context :uri) "#local:"
                             (definition-start :line) ":"
                             (definition-start :character)))]
      (each reference (record :references)
        (when (= identity (reference :identity))
          (array/push locations {:uri (context :uri) :range (reference :range)})))
      (when (and include-declaration
                 (not (any? (map |(server-utils/same-position?
                                    definition-start (get-in $ [:range :start]))
                                 locations))))
        (array/push locations {:uri (context :uri)
                               :range (get-in context [:local :range])})))
    (each reference
          (index/references-by-identity (context :workspace)
                                        (get-in context [:indexed :identity]))
      (array/push locations {:uri (reference :uri) :range (reference :range)})))
  (unless include-declaration
    (def definition (or (context :local) (context :indexed)))
    (def definition-uri (if (context :local) (context :uri)
                          (and definition (definition :uri))))
    (def definition-position
      (get-in definition [(if (context :local) :range :selection-range) :start]))
    (set locations
         (filter |(not (and (= definition-uri ($ :uri))
                            (server-utils/same-position?
                              definition-position (get-in $ [:range :start]))))
                 locations)))
  locations)

(defn references [state params]
  (def context (symbol-context state params))
  (if (or (empty? (context :name))
          (not (or (context :local) (context :indexed))))
    @[]
    (distinct
      (catseq [found :in (raw-references
                           context
                           (not= false (get-in params ["context" "includeDeclaration"])))
               :let [content (server-utils/content state (found :uri))]
               :when content]
        {:uri (found :uri)
         :range (server-utils/lsp-range state content (found :range))}))))

(defn on-references [state params]
  [:ok state (references state params)])

(defn on-document-highlights [state params]
  (def context (symbol-context state params))
  (if (or (empty? (context :name))
          (not (or (context :local) (context :indexed))))
    [:ok state @[]]
    (let [document-uri (context :uri)
          definition (or (context :local) (context :indexed))
          definition-uri (if (context :local)
                           document-uri
                           (definition :uri))
          definition-start
          (get-in definition [(if (context :local) :range :selection-range) :start])
          highlights
          (catseq [reference :in (raw-references context true)
                   :when (= document-uri (reference :uri))]
            {:range (server-utils/lsp-range state (context :content)
                                           (reference :range))
             :kind (if (and (= definition-uri document-uri)
                            (server-utils/same-position?
                              definition-start
                              (get-in reference [:range :start])))
                     3
                     2)})]
      [:ok state
       (sort-by |[(get-in $ [:range :start :line])
                  (get-in $ [:range :start :character])]
                (distinct highlights))])))

(varfn import-rename-target [])
(varfn alias-occurrence-target [])

(defn- rename-target [state params]
  (or (import-rename-target state params)
      (alias-occurrence-target state params)
      (let [context (symbol-context state params)]
        (when (and (or (context :local) (context :indexed))
                   (not (get-in context [:indexed :generated])))
          (def canonical (or (get-in context [:indexed :name]) (context :name)))
          (def word-start (first (get-in context [:word :range])))
          (def suffix-start (- (last (get-in context [:word :range]))
                               (length canonical)))
          (when (and (string/has-suffix? canonical (context :name))
                     (<= suffix-start (get-in context [:location :character])))
            {:kind :symbol :context context
             :name canonical
             :content (context :content)
             :range {:start {:line (get-in context [:location :line])
                             :character (max word-start suffix-start)}
                     :end {:line (get-in context [:location :line])
                           :character (last (get-in context [:word :range]))}}})))))

(defn- valid-symbol? [name]
  (try (let [parsed (parse name)]
         (and (symbol? parsed) (= name (string parsed))))
    ([_] false)))

(defn- byte-range [state content range]
  (when-let [start (position/lsp->byte-position content (get range "start")
                                                 (state :position-encoding))
             end (position/lsp->byte-position content (get range "end")
                                               (state :position-encoding))]
    {:start (lookup/to-index start content) :end (lookup/to-index end content)}))

(defn- token-range [token content]
  {:start (lookup/from-index (token :start) content)
   :end (lookup/from-index (token :end) content)})

(defn- import-syntax [content imported]
  (def scanned (document-features/scan content))
  (def start (lookup/to-index (get-in imported [:range :start]) content))
  (def end (lookup/to-index (get-in imported [:range :end]) content))
  (when-let [form (first (filter |(and (= start ($ :start)) (= end ($ :end))
                                       (= 40 ($ :open)))
                                 (scanned :forms)))]
    (def direct
      (sort-by |($ :start)
               (filter |(= (form :id) ($ :parent)) (scanned :tokens))))
    (def module-token
      (if (= "use" (imported :kind))
        (first (filter |(= (imported :module) ($ :value)) direct))
        (get direct 1)))
    (defn option-value [key]
      (when-let [option (find-index |(= key ($ :value)) direct)]
        (get direct (inc option))))
    (def only-form
      (when-let [option (find-index |(= ":only" ($ :value)) direct)]
        (first (filter |(and (= (form :id) ($ :parent))
                             (> ($ :start) (get-in direct [option :end]))
                             (= 91 ($ :open)))
                       (scanned :forms)))))
    {:module module-token
     :as (option-value ":as")
     :prefix (option-value ":prefix")
     :only (if only-form
             (filter |(= (only-form :id) ($ :parent)) (scanned :tokens))
             @[])}))

(defn- import-context [state params]
  (when-let [document (server-utils/document state params)]
    (def content (document :content))
    (def location (server-utils/request-byte-position state params content))
    (def cursor (lookup/to-index location content))
    (some
      (fn [imported]
        (when-let [syntax (import-syntax content imported)]
          (def found
            (some (fn [kind]
                    (when (or (not= kind :prefix) (nil? (syntax :as)))
                      (some |(and (<= ($ :start) cursor ($ :end))
                                  {:kind kind :token $})
                            (if (= kind :only) (syntax :only)
                              (if (syntax kind) [(syntax kind)] @[])))))
                  [:module :as :prefix :only]))
          (and found
               (merge found {:document document :content content
                             :workspace (server-utils/document-workspace state document)
                             :import imported :syntax syntax}))))
      (get-in document [:analysis :index :imports] @[]))))

(defn- import-member-definition [context]
  (when-let [target-uri
             (let [targets
                   (index/module-uris (context :workspace)
                                      (get-in context [:document :uri])
                                      (get-in context [:import :module]))]
               (and (= 1 (length targets)) (targets 0)))]
    (def name (get-in context [:token :value]))
    (first (filter |(= name ($ :name))
                   (index/exported-definitions (context :workspace) target-uri)))))

(defn- identity-locations [state workspace identity]
  (def locations @[])
  (each reference (index/references-by-identity workspace identity)
    (array/push locations {:uri (reference :uri) :range (reference :range)
                           :qualified (string/find "/" (reference :name))}))
  (each record (values (workspace :index))
    (when-let [content (server-utils/content state (record :uri))]
      (each imported (record :imports)
        (when-let [target-uri
                   (let [targets (index/module-uris workspace (record :uri)
                                                    (imported :module))]
                     (and (= 1 (length targets)) (targets 0)))]
          (each token (get-in (import-syntax content imported) [:only] @[])
            (when-let [definition
                       (first (filter |(= (token :value) ($ :name))
                                      (index/exported-definitions workspace target-uri)))]
              (when (= identity (definition :identity))
                (array/push locations
                            {:uri (record :uri)
                             :range (token-range token content)}))))))))
  (distinct locations))

(varfn alias-occurrence-target [state params]
  (when-let [document (server-utils/document state params)]
    (def content (document :content))
    (def location (server-utils/request-byte-position state params content))
    (def word (lookup/word-at location content))
    (def name (word :word))
    (def word-start (first (word :range)))
    (def offset (- (location :character) word-start))
    (some
      (fn [imported]
        (when-let [syntax (import-syntax content imported)]
          (def explicit (or (syntax :as) (syntax :prefix)))
          (def prefix (imported :prefix))
          (def editable-length
            (if (syntax :as) (length (imported :alias)) (length prefix)))
          (def targets
            (index/module-uris (server-utils/document-workspace state document)
                               (document :uri) (imported :module)))
          (when (and (= 1 (length targets)) explicit (not (empty? prefix))
                     (string/has-prefix? prefix name)
                     (<= 0 offset) (< offset editable-length)
                     (any? (map |(and (= name ($ :name))
                                      (server-utils/same-position?
                                        (get-in $ [:range :start])
                                        {:line (location :line)
                                         :character word-start}))
                                 (get-in document
                                         [:analysis :index :references]))))
            {:kind :alias
             :name (if (syntax :as) (imported :alias) prefix)
             :range {:start {:line (location :line) :character word-start}
                     :end {:line (location :line)
                           :character (+ word-start editable-length)}}
             :context {:document document :content content
                       :workspace (server-utils/document-workspace state document)
                       :import imported :syntax syntax
                       :kind (if (syntax :as) :as :prefix)
                       :token explicit}})))
      (get-in document [:analysis :index :imports] @[]))))

(varfn import-rename-target [state params]
  (when-let [context (import-context state params)]
    (cond
      (has-value? [:as :prefix] (context :kind))
      (let [targets
            (index/module-uris (context :workspace)
                               (get-in context [:document :uri])
                               (get-in context [:import :module]))]
        (when (and (= 1 (length targets))
                   (not (empty? (get-in context [:import :prefix]))))
          {:kind :alias :context context
           :name (if (= :prefix (context :kind))
                   (get-in context [:import :prefix])
                   (get-in context [:import :alias]))
           :range (token-range (context :token) (context :content))}))

      (= :only (context :kind))
      (when-let [definition (import-member-definition context)]
        {:kind :member :context context :definition definition
         :name (get-in context [:token :value])
         :range (token-range (context :token) (context :content))})

      (= :module (context :kind))
      (when (and (state :rename-file-support) (state :document-changes-support)
                 (= "import" (get-in context [:import :kind]))
                 (string/has-prefix? "." (get-in context [:import :module]))
                 (get-in context [:workspace :path]))
        (def module (get-in context [:import :module]))
        (def basename (path/basename module))
        (def extension (path/ext basename))
        (def stem (if extension
                    (string/slice basename 0 (- (length basename)
                                                (length extension)))
                    basename))
        (def token (context :token))
        (def start (+ (token :start) (- (length module) (length basename))))
        (when (= module (string/slice (context :content)
                                      (token :start) (token :end)))
          {:kind :module :context context :name stem
           :range {:start (lookup/from-index start (context :content))
                   :end (lookup/from-index (+ start (length stem))
                                           (context :content))}}))
      nil)))

(defn- lsp-edit [state uri range new-text]
  (when-let [content (server-utils/content state uri)]
    {:range (server-utils/lsp-range state content range) :newText new-text}))

(defn- changes-edit [state changes uri range new-text]
  (when-let [edit (lsp-edit state uri range new-text)]
    (unless (get changes uri) (put changes uri @[]))
    (array/push (get changes uri) edit)))

(defn- document-changes [state changes]
  (map (fn [document-uri]
         (server-utils/versioned-edit
           state document-uri
           (sort-by |[(get-in $ [:range :start :line])
                      (get-in $ [:range :start :character])
                      (get-in $ [:range :end :line])
                      (get-in $ [:range :end :character])]
                    (distinct (get changes document-uri)))))
       (sort (keys changes))))

(defn- alias-rename [state target new-name]
  (def context (target :context))
  (def imported (context :import))
  (def record (get-in context [:document :analysis :index]))
  (def old-prefix (imported :prefix))
  (def new-prefix (if (= :prefix (context :kind)) new-name
                    (string new-name "/")))
  (def competing
    (filter |(and (not (deep= ($ :range) (imported :range)))
                  (or (= old-prefix ($ :prefix)) (= new-prefix ($ :prefix))))
            (record :imports)))
  (def target-uri
    (let [targets (index/module-uris (context :workspace) (record :uri)
                                    (imported :module))]
      (and (= 1 (length targets)) (targets 0))))
  (def renamed-bindings
    (and target-uri
         (catseq [definition :in
                  (index/exported-definitions (context :workspace) target-uri)
                  :when (or (empty? (imported :only))
                            (has-value? (imported :only) (definition :name)))]
           (string new-prefix (definition :name)))))
  (def existing-bindings
    (array
      ;(map |($ :name) (record :definitions))
      ;(catseq [other :in (record :imports)
                :when (not (deep= (other :range) (imported :range)))
                :let [uri (index/module-uri (context :workspace) (record :uri)
                                            (other :module))]
                :when uri
                definition :in (index/exported-definitions (context :workspace) uri)
                :when (or (empty? (other :only))
                          (has-value? (other :only) (definition :name)))]
          (string (other :prefix) (definition :name)))))
  (when (and (not (empty? old-prefix)) renamed-bindings (empty? competing)
             (not (any? (map |(has-value? existing-bindings $) renamed-bindings))))
    (def changes @{})
    (def token (context :token))
    (def declaration-text (if (= :prefix (context :kind)) new-prefix new-name))
    (changes-edit state changes (get-in context [:document :uri])
                  (token-range token (context :content)) declaration-text)
    (each reference (record :references)
      (when (and (= :import (reference :identity-kind))
                 (string/has-prefix? old-prefix (reference :name))
                 (not (server-utils/same-position?
                        (get-in imported [:range :start])
                        (get-in reference [:range :start]))))
        (changes-edit state changes (record :uri) (reference :range)
                      (string new-prefix
                              (string/slice (reference :name)
                                            (length old-prefix))))))
    changes))

(defn- identity-collision? [workspace identity new-name]
  (def origin (first (filter |(= identity ($ :identity))
                             (index/definitions workspace))))
  (or
    (and origin
         (some |(and (= new-name ($ :name)) (not= identity ($ :identity)))
               (get-in workspace [:index (origin :uri) :definitions] @[])))
    (some
      (fn [record]
        (some
          (fn [imported]
            (when-let [target-uri
                       (let [targets (index/module-uris workspace (record :uri)
                                                        (imported :module))]
                         (and (= 1 (length targets)) (targets 0)))
                       definition
                       (first (filter |(= identity ($ :identity))
                                      (index/exported-definitions workspace target-uri)))]
              (when (or (empty? (imported :only))
                        (has-value? (imported :only) (definition :name)))
                (def candidate (string (imported :prefix) new-name))
                (or (some |(= candidate ($ :name)) (record :definitions))
                    (some
                      (fn [other]
                        (when (not (deep= (other :range) (imported :range)))
                          (when-let [uri
                                     (let [targets
                                           (index/module-uris workspace (record :uri)
                                                              (other :module))]
                                       (and (= 1 (length targets)) (targets 0)))]
                            (some |(and (or (empty? (other :only))
                                           (has-value? (other :only) ($ :name)))
                                        (= candidate
                                           (string (other :prefix) ($ :name))))
                                  (index/exported-definitions workspace uri)))))
                      (record :imports))))))
          (record :imports)))
      (values (workspace :index)))))

(defn- member-rename [state target new-name]
  (def definition (target :definition))
  (def workspace (get-in target [:context :workspace]))
  (unless (identity-collision? workspace (definition :identity) new-name)
    (def changes @{})
    (each location (identity-locations state workspace (definition :identity))
      (def old-name
        (when-let [content (server-utils/content state (location :uri))]
          (def range (location :range))
          (def start (lookup/to-index (range :start) content))
          (def end (lookup/to-index (range :end) content))
          (string/slice content start end)))
      (when old-name
        (changes-edit state changes (location :uri) (location :range)
                      (if (string/has-suffix? (definition :name) old-name)
                        (string (string/slice old-name 0
                                              (- (length old-name)
                                                 (length (definition :name))))
                                new-name)
                        new-name))))
    changes))

(defn- source-module-path [document-path target-path extension?]
  (when (and document-path target-path (not= document-path target-path))
    (var relative
      (string/replace-all "\\" "/"
                          (path/relpath (path/dirname document-path) target-path)))
    (when (and (not extension?) (string/has-suffix? ".janet" relative))
      (set relative (string/slice relative 0 (- (length relative) 6))))
    (if (or (string/has-prefix? "." relative)
            (string/has-prefix? "/" relative))
      relative
      (string "./" relative))))

(defn- module-rename [state target new-name]
  (def context (target :context))
  (def workspace (context :workspace))
  (when-let [old-uri
             (let [targets
                   (index/module-uris workspace
                                      (get-in context [:document :uri])
                                      (get-in context [:import :module]))]
               (and (= 1 (length targets)) (targets 0)))
             old-path (uri/file-uri->path old-uri)]
    (def old-base (path/basename old-path))
    (def extension (path/ext old-base))
    (def new-path (path/join (path/dirname old-path)
                             (string new-name extension)))
    (def new-uri (uri/path->file-uri new-path))
    (def reserved-name
      (has-value? ["CON" "PRN" "AUX" "NUL"
                   "COM1" "COM2" "COM3" "COM4" "COM5"
                   "COM6" "COM7" "COM8" "COM9"
                   "LPT1" "LPT2" "LPT3" "LPT4" "LPT5"
                   "LPT6" "LPT7" "LPT8" "LPT9"]
                  (string/ascii-upper new-name)))
    (when (and (not (any? (map |(string/find $ new-name)
                               ["/" "\\" "<" ">" ":" "\"" "|" "?" "*"])))
               (not (has-value? ["." ".."] new-name))
               (not reserved-name)
               (not (string/has-suffix? ".janet" new-name))
               (= ".janet" extension)
               (not= "init.janet" old-base)
               (not (os/stat new-path))
               (nil? (get-in state [:documents new-uri]))
               (not (get-in workspace [:index new-uri]))
               (server-utils/path-in-workspace? new-path (workspace :path)))
      (def changes @{})
      (var safe true)
      (each record (values (workspace :index))
        (when-let [content (server-utils/content state (record :uri))]
          (each imported (record :imports)
            (def targets (index/module-uris workspace (record :uri)
                                            (imported :module)))
            (when (has-value? targets old-uri)
              (when (> (length (filter |(and (= (imported :module) ($ :module))
                                               (deep= (imported :range) ($ :range)))
                                         (record :imports)))
                       1)
                (set safe false))
              (when (not= 1 (length targets)) (set safe false))
              (if (not (string/has-prefix? "." (imported :module)))
                (set safe false)
                (if-let [document-path (uri/file-uri->path (record :uri))
                         module (source-module-path
                                  document-path new-path
                                  (string/has-suffix? ".janet" (imported :module)))
                         token (get-in (import-syntax content imported) [:module])]
                  (changes-edit state changes (record :uri)
                                (token-range token content) module)
                  (set safe false)))
              (when (and (= 1 (length targets))
                         (has-value? ["import" "import*"] (imported :kind))
                         (nil? (get-in (import-syntax content imported) [:as]))
                         (nil? (get-in (import-syntax content imported) [:prefix])))
                (def old-prefix (imported :prefix))
                (def new-prefix (string new-name "/"))
                (def imported-definitions
                  (filter |(or (empty? (imported :only))
                               (has-value? (imported :only) ($ :name)))
                          (index/exported-definitions workspace old-uri)))
                (def new-bindings
                  (map |(string new-prefix ($ :name)) imported-definitions))
                (def existing-bindings
                  (array
                    ;(map |($ :name) (record :definitions))
                    ;(catseq [other :in (record :imports)
                              :when (not (deep= (other :range) (imported :range)))
                              :let [uris (index/module-uris workspace (record :uri)
                                                           (other :module))]
                              :when (= 1 (length uris))
                              definition :in
                              (index/exported-definitions workspace (uris 0))
                              :when (or (empty? (other :only))
                                        (has-value? (other :only)
                                                    (definition :name)))]
                      (string (other :prefix) (definition :name)))))
                (when (any? (map |(has-value? existing-bindings $) new-bindings))
                  (set safe false))
                (each reference (record :references)
                  (when (and (= :import (reference :identity-kind))
                             (string/has-prefix? old-prefix (reference :name)))
                    (def target-name
                      (string/slice (reference :name) (length old-prefix)))
                    (def imported-definition
                      (first (filter |(and (= target-name ($ :name))
                                           (= (reference :identity) ($ :identity)))
                                     (index/exported-definitions workspace old-uri))))
                    (when imported-definition
                      (changes-edit state changes (record :uri) (reference :range)
                                    (string new-prefix target-name))))))))))
      (when safe
        {:changes changes :operation {:kind "rename" :oldUri old-uri :newUri new-uri
                                      :options {:overwrite false
                                                :ignoreIfExists false}}}))))

(defn on-prepare-rename [state params]
  (if-let [target (rename-target state params)]
    [:ok state {:range (server-utils/lsp-range
                         state (or (target :content)
                                   (get-in target [:context :content]))
                         (target :range))
                 :placeholder (target :name)}]
    [:rpc-error state -32602 "Invalid params" "symbol cannot be renamed"]))

(defn on-rename [state params]
  (def new-name (get params "newName"))
  (def target (rename-target state params))
  (cond
    (not (valid-symbol? new-name))
    [:rpc-error state -32602 "Invalid params" "newName must be one Janet symbol"]

    (nil? target)
    [:rpc-error state -32602 "Invalid params" "symbol cannot be renamed"]

    (and (= :symbol (target :kind))
         (get-in target [:context :indexed :identity])
         (identity-collision? (get-in target [:context :workspace])
                              (get-in target [:context :indexed :identity])
                              new-name))
    [:rpc-error state -32602 "Invalid params" "newName conflicts with a binding"]

    (= :module (target :kind))
    (if-let [renamed (module-rename state target new-name)]
      [:ok state
       {:documentChanges
        (array
          ;(document-changes state (renamed :changes))
          ;[(renamed :operation)])}]
      [:rpc-error state -32602 "Invalid params" "module cannot be renamed safely"])

    (has-value? [:alias :member] (target :kind))
    (if-let [changes (and (or (not= :alias (target :kind))
                              (not (string/find "/" new-name)))
                          ((if (= :alias (target :kind))
                             alias-rename member-rename)
                           state target new-name))]
      [:ok state
       {:documentChanges (document-changes state changes)}]
      [:rpc-error state -32602 "Invalid params" "import cannot be renamed safely"])

    (let [context (target :context)
          changes @{}
          locations
          (if-let [identity (get-in context [:indexed :identity])]
            (identity-locations state (context :workspace) identity)
            (map |{:uri ($ :uri) :range ($ :range)}
                 (raw-references context true)))]
      (each location locations
        (def document-uri (location :uri))
        (when-let [content (server-utils/content state document-uri)]
          (def range (location :range))
          (def start (lookup/to-index (range :start) content))
          (def end (lookup/to-index (range :end) content))
          (def old-name (string/slice content start end))
          (def canonical (or (get-in context [:indexed :name]) (context :name)))
          (changes-edit state changes document-uri range
                        (if (string/has-suffix? canonical old-name)
                          (string (string/slice old-name 0
                                                (- (length old-name)
                                                   (length canonical)))
                                  new-name)
                          new-name))))
      [:ok state
       {:documentChanges (document-changes state changes)}])))
