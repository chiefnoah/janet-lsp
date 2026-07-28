(import spork/path)
(import ./lookup)
(import ./parser)
(import ./uri)

(def default-exclusions [".git" ".hg" ".svn" "build" "dist" "node_modules" "jpm_tree"])
(def definition-heads
  {"def" 13 "def-" 13 "var" 13 "var-" 13
   "defn" 12 "defn-" 12 "varfn" 12 "varfn-" 12
   "defmacro" 12 "defmacro-" 12})
(def function-heads
  {"defn" true "defn-" true "varfn" true "varfn-" true
   "defmacro" true "defmacro-" true})
(def import-heads
  {"import" true "import*" true "use" true "require" true "dofile" true})

(varfn definitions [])

(defn- node-range [node source]
  {:start (lookup/from-index (node :index) source)
   :end (lookup/from-index (+ (node :index) (node :len)) source)})

(defn- leaf? [node]
  (and (dictionary? node) (string? (node :value))))

(defn- form? [node]
  (and (dictionary? node) (has-value? [:ptuple :def :defn :lambda] (node :tag))
       (indexed? (node :value)) (not (empty? (node :value)))))

(defn- head [node source]
  (when (form? node)
    (def fragment
      (string/slice (lookup/code-mask source)
                    (node :index) (+ (node :index) (node :len))))
    (when-let [value
               (get-in (first (filter |(not (empty? ($ 1)))
                                      (or (peg/match lookup/word-peg fragment) @[])))
                       [1])]
      (string/trim value))))

(varfn collect-binding-leaves [])

(varfn collect-binding-leaves [node leaves]
  (cond
    (leaf? node)
    (when (and (not (string/has-prefix? "&" (node :value)))
               (not (string/has-prefix? "_" (node :value)))
               (not (string/has-prefix? ":" (node :value))))
      (array/push leaves node))
    (and (dictionary? node) (indexed? (node :value)))
    (each child (node :value) (collect-binding-leaves child leaves)))
  leaves)

(defn- binding-leaves [node]
  (collect-binding-leaves node @[]))

(defn- parameter-node [form]
  (first (filter |(has-value? [:btuple :parameters] ($ :tag)) (form :value))))

(defn- definition-name-node [form]
  (case (form :tag)
    :def (get-in (first (filter |(= :variables ($ :tag)) (form :value))) [:value 0])
    :defn (get-in (first (filter |(= :fn ($ :tag)) (form :value))) [:value 0])
    (get-in form [:value 1])))

(defn- definition [form document-uri source top-level container]
  (def form-head (head form source))
  (when-let [kind (get definition-heads form-head)
             name-node (definition-name-node form)
             name (and (leaf? name-node) (name-node :value))]
    (def selection-range (node-range name-node source))
    (def children
      (if (get function-heads form-head)
        (if-let [parameters (parameter-node form)]
          (map |{:name ($ :value)
                 :kind 13
                 :range (node-range $ source)
                 :selection-range (node-range $ source)}
               (binding-leaves parameters))
          @[])
        @[]))
    {:name name
     :uri document-uri
     :identity (string document-uri "#" name "@"
                       (get-in selection-range [:start :line]) ":"
                       (get-in selection-range [:start :character]))
     :kind kind
     :form form-head
     :top-level top-level
     :container container
     :range (node-range form source)
     :selection-range selection-range
     :children children}))

(defn- import-record [form source]
  (def form-head (head form source))
  (when (get import-heads form-head)
    (def leaves (filter leaf? (form :value)))
    (def parsed
      (try (parse (string/slice source (form :index)
                                (+ (form :index) (form :len))))
        ([_] nil)))
    (def module
      (or (and (> (length leaves) 1) ((leaves 1) :value))
          (and (indexed? parsed) (> (length parsed) 1)
               (string (parsed 1)))))
    (when module
      (def alias-index (find-index |(= ":as" ($ :value)) leaves))
      (def only-index (find-index |(= ":only" ($ :value)) leaves))
      (def export-index (find-index |(= ":export" ($ :value)) leaves))
      {:module module
       :kind form-head
       :alias (if (and alias-index (< (inc alias-index) (length leaves)))
                ((leaves (inc alias-index)) :value)
                (path/basename module))
       :only (if only-index
               (let [node (get-in form [:value (inc only-index)])]
                 (if node (map |($ :value) (binding-leaves node)) @[]))
               @[])
       :export (and export-index
                    (= "true" (get-in leaves [(inc export-index) :value])))
       :range (node-range form source)})))

(defn- collect-nodes
  [node document-uri source definitions references imports depth container]
  (when (dictionary? node)
    (def found (definition node document-uri source (= depth 0) container))
    (when found (array/push definitions found))
    (when-let [found (import-record node source)]
      (array/push imports found))
    (when (leaf? node)
      (array/push references
                  @{:name (node :value) :uri document-uri
                    :range (node-range node source)}))
    (when (indexed? (node :value))
      (each child (node :value)
        (collect-nodes child document-uri source definitions references imports
                       (if (form? node) (inc depth) depth)
                       (if found (found :identity) container))))))

(defn- same-position? [a b]
  (and a b (= (a :line) (b :line)) (= (a :character) (b :character))))

(defn analyze [document-uri content]
  (def definitions @[])
  (def references @[])
  (def imports @[])
  (try
    (each node ((parser/syntax-tree content) :value)
      (collect-nodes node document-uri content definitions references imports 0 nil))
    ([_] nil))
  (each reference references
    (def declared
      (first (filter |(same-position? (get-in reference [:range :start])
                                      (get-in $ [:selection-range :start]))
                     definitions)))
    (if declared
      (do
        (put reference :identity (declared :identity))
        (put reference :identity-kind :definition))
      (when-let [resolved
                 (parser/definition-at (get-in reference [:range :start])
                                       content (reference :name))]
        (def definition
          (first (filter |(same-position? (get-in resolved [:range :start])
                                          (get-in $ [:selection-range :start]))
                         definitions)))
        (put reference :identity
             (if definition
               (definition :identity)
               (string document-uri "#local:"
                       (get-in resolved [:range :start :line]) ":"
                       (get-in resolved [:range :start :character]))))
        (put reference :identity-kind (if definition :definition :local)))))
  {:uri document-uri
   :definitions definitions
   :references references
   :imports imports})

(defn- module-candidates [document-uri module]
  (when-let [document-path (uri/file-uri->path document-uri)]
    (def base (path/abspath (path/join (path/dirname document-path) module)))
    [(uri/path->file-uri base)
     (uri/path->file-uri (string base ".janet"))
     (uri/path->file-uri (path/join base "init.janet"))]))

(defn- module-uri [workspace document-uri module]
  (or (and (string/has-prefix? "." module)
           (first (filter |(get (workspace :index) $)
                          (module-candidates document-uri module))))
      (first
        (filter
          (fn [candidate]
            (when-let [candidate-path (uri/file-uri->path candidate)]
              (or (string/has-suffix? (string "/" module ".janet") candidate-path)
                  (string/has-suffix? (string "/" module "/init.janet") candidate-path))))
          (keys (workspace :index))))))

(defn- definition-in [workspace document-uri name]
  (first (filter |(and ($ :top-level) (= name ($ :name)))
                 (get-in workspace [:index document-uri :definitions] @[]))))

(varfn exported-definition [])

(varfn exported-definition [workspace document-uri name visited]
  (def key (string document-uri "#" name))
  (unless (get visited key)
    (put visited key true)
    (or (first (filter |(and ($ :top-level)
                             (= name ($ :name))
                             (not (string/has-suffix? "-" ($ :form))))
                       (get-in workspace [:index document-uri :definitions] @[])))
        (some (fn [imported]
                (when (and (imported :export)
                           (= "use" (imported :kind))
                           (or (empty? (imported :only))
                               (has-value? (imported :only) name)))
                  (when-let [target-uri
                             (module-uri workspace document-uri (imported :module))]
                    (exported-definition workspace target-uri name visited))))
              (get-in workspace [:index document-uri :imports] @[])))))

(defn resolve-definition [workspace document-uri name]
  (def parts (string/split "/" name))
  (def record (get (workspace :index) document-uri))
  (if (> (length parts) 1)
    (let [target-name (last parts)
          prefix (first parts)]
      (or (when-let [imported
                     (first (filter |(= prefix ($ :alias)) (record :imports)))
                     target-uri (module-uri workspace document-uri (imported :module))]
            (when (or (empty? (imported :only))
                      (has-value? (imported :only) target-name))
              (exported-definition workspace target-uri target-name @{})))
          (first (filter |(not (string/has-suffix? "-" ($ :form)))
                         (definitions workspace target-name)))))
    (or (definition-in workspace document-uri name)
        (some (fn [imported]
                (when (and (= "use" (imported :kind))
                           (or (empty? (imported :only))
                               (has-value? (imported :only) name)))
                  (when-let [target-uri
                             (module-uri workspace document-uri (imported :module))]
                    (exported-definition workspace target-uri name @{}))))
              (record :imports)))))

(defn relink [workspace]
  (each record (values (workspace :index))
    (each reference (record :references)
      (when (= :import (reference :identity-kind))
        (put reference :identity nil)
        (put reference :identity-kind nil))
      (unless (reference :identity)
        (when-let [definition
                   (resolve-definition workspace (record :uri) (reference :name))]
          (put reference :identity (definition :identity))
          (put reference :identity-kind :import)))))
  workspace)

(defn update [workspace document-uri content]
  (put (workspace :index) document-uri (analyze document-uri content))
  (relink workspace))

(defn add-generated [workspace document-uri env]
  (when-let [record (get (workspace :index) document-uri)
             document-path (uri/file-uri->path document-uri)]
    (each name (all-bindings env)
      (def binding (get env name))
      (when-let [[source-path line column]
                 (and (dictionary? binding) (binding :source-map))]
        (when (and (string? source-path)
                   (= (path/abspath source-path) (path/abspath document-path))
                   (not (any? (map |(and ($ :top-level)
                                        (= (string name) ($ :name)))
                                   (record :definitions)))))
          (def location {:line (max 0 (dec (or line 1)))
                         :character (max 0 (dec (or column 1)))})
          (array/push
            (record :definitions)
            {:name (string name)
             :uri document-uri
             :identity (string document-uri "#generated:" name)
             :kind (if (has-value? [:function :cfunction]
                                   (type (binding :value))) 12 13)
             :form "generated"
             :generated true
             :top-level true
             :range {:start location :end location}
             :selection-range {:start location :end location}
             :children @[]})))))
  (relink workspace))

(defn remove [workspace document-uri]
  (put (workspace :index) document-uri nil)
  (relink workspace))

(varfn definitions [workspace &opt name]
  (catseq [record :in (values (workspace :index))
           definition :in (record :definitions)
           :when (and (definition :top-level)
                      (or (nil? name) (= name (definition :name))))]
    definition))

(defn references-by-identity [workspace identity]
  (catseq [record :in (values (workspace :index))
           reference :in (record :references)
           :when (= identity (reference :identity))]
    reference))

(defn scan [root exclusions]
  (unless (os/stat root)
    (error (string "workspace root does not exist: " root)))
  (def records @{})
  (def pending @[root])
  (while (not (empty? pending))
    (def current (array/pop pending))
    (case (os/stat current :mode)
      :directory
      (unless (has-value? exclusions (path/basename current))
        (each entry (os/dir current) (array/push pending (path/join current entry))))
      :file
      (when (and (string/has-suffix? ".janet" current)
                 (not (any? (map |(has-value? exclusions $)
                                 (string/split "/" current)))))
        (try
          (do
            (def document-uri (uri/path->file-uri current))
            (put records document-uri (analyze document-uri (slurp current))))
          ([_] nil)))))
  (def workspace @{:index records})
  (relink workspace)
  (workspace :index))
