(import spork/path)
(import ./lookup)
(import ./parser)
(import ./uri)

(def default-exclusions
  [".git" ".hg" ".svn" ".direnv" "build" "dist" "node_modules" "jpm_tree"])
(def definition-heads
  {"def" 13 "def-" 13 "defglobal" 13
   "var" 13 "var-" 13 "varglobal" 13
   "defn" 12 "defn-" 12 "varfn" 12 "varfn-" 12
   "defmacro" 12 "defmacro-" 12})
(def function-heads
  {"defn" true "defn-" true "varfn" true "varfn-" true
   "defmacro" true "defmacro-" true})
(def import-heads
  {"import" true "import*" true "use" true "require" true "dofile" true})

(varfn definitions [])

(defn content-hash [content]
  (if (> (length content) 262144)
    (string "large:" (length content) ":" (hash content))
    (let [offset (int/u64 "14695981039346656037")
          prime (int/u64 "1099511628211")]
      (var fnv-1a offset)
      (var fnv-1 offset)
      (each byte (string/bytes content)
        (def value (int/u64 byte))
        (set fnv-1a (* (bxor fnv-1a value) prime))
        (set fnv-1 (bxor (* fnv-1 prime) value)))
      (string fnv-1a "-" fnv-1))))

(defn- node-range [node source line-starts]
  {:start (lookup/from-index (node :index) source line-starts)
   :end (lookup/from-index (+ (node :index) (node :len)) source line-starts)})

(defn- leaf? [node]
  (and (dictionary? node) (string? (node :value))))

(defn- form? [node]
  (and (dictionary? node) (has-value? [:ptuple :def :defn :lambda] (node :tag))
       (indexed? (node :value)) (not (empty? (node :value)))))

(defn- head [node source mask]
  (when (form? node)
    (def fragment
      (string/slice mask
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

(defn- parsed-parameter-index [parsed]
  (find-index |(and (tuple? $) (= :brackets (tuple/type $))) parsed))

(defn- private-definition? [form source form-head]
  (try
    (let [parsed (parse (string/slice source (form :index)
                                      (+ (form :index) (form :len))))
          metadata
          (if (get function-heads form-head)
            (let [parameters
                   (parsed-parameter-index parsed)]
              (if parameters (tuple/slice parsed 2 parameters) @[]))
            (tuple/slice parsed 2 (dec (length parsed))))]
      (has-value? metadata :private))
    ([_] false)))

(defn- parsed-definition-metadata [form source form-head]
  (try
    (let [parsed (parse (string/slice source (form :index)
                                      (+ (form :index) (form :len))))]
      (if (get function-heads form-head)
        (if-let [parameters (parsed-parameter-index parsed)]
          (tuple/slice parsed 2 parameters)
          @[])
        (tuple/slice parsed 2 (dec (length parsed)))))
    ([_] @[])))

(defn- definition-metadata [form source form-head target-range]
  (def parsed-metadata (parsed-definition-metadata form source form-head))
  (def type-name
    (some |(and (dictionary? $) (get $ :janet-lsp/type-definition))
          parsed-metadata))
  (def implementation-value
    (some |(and (dictionary? $) (get $ :janet-lsp/implements))
           parsed-metadata))
  (def return-value
    (some |(and (dictionary? $) (get $ :janet-lsp/returns))
          parsed-metadata))
  (def implementation-names
    (cond
      (string? implementation-value) [implementation-value]
      (and (indexed? implementation-value) (all string? implementation-value))
      (array ;implementation-value)
      @[]))
  {:type-target (when (and (string? type-name) (not (empty? type-name)))
                   {:name type-name :range target-range})
   :return-target (when (and (string? return-value) (not (empty? return-value)))
                    return-value)
   :implementation-targets
   (map |{:name $ :range target-range}
        (filter |(not (empty? $)) implementation-names))})

(defn- definition [form document-uri source mask line-starts top-level container]
  (def form-head (head form source mask))
  (when-let [kind (get definition-heads form-head)
             name-node (definition-name-node form)
             name (and (leaf? name-node) (name-node :value))]
    (def selection-range (node-range name-node source line-starts))
    (def metadata (definition-metadata form source form-head selection-range))
    (def children
      (if (get function-heads form-head)
        (if-let [parameters (parameter-node form)]
          (map |{:name ($ :value)
                 :kind 13
                  :range (node-range $ source line-starts)
                  :selection-range (node-range $ source line-starts)}
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
     :private (private-definition? form source form-head)
      :type-target (metadata :type-target)
      :return-target (metadata :return-target)
     :implementation-targets (metadata :implementation-targets)
     :top-level top-level
     :container container
     :range (node-range form source line-starts)
     :selection-range selection-range
     :children children}))

(defn- parsed-option [parsed key]
  (when (indexed? parsed)
    (when-let [option (find-index |(= key $) parsed)]
      (get parsed (inc option)))))

(defn- import-records [form source mask line-starts top-level]
  (def form-head (head form source mask))
  (when (get import-heads form-head)
    (def parsed
      (try (parse (string/slice source (form :index)
                                (+ (form :index) (form :len))))
        ([_] nil)))
    (when (and (indexed? parsed) (> (length parsed) 1))
      (def modules
        (if (= "use" form-head)
          (tuple/slice parsed 1)
          [(parsed 1)]))
      (def as (parsed-option parsed :as))
      (def explicit-prefix (parsed-option parsed :prefix))
      (def only (parsed-option parsed :only))
      (def exported (not (not (parsed-option parsed :export))))
      (catseq [module-value :in modules
               :when (or (symbol? module-value) (string? module-value))
               :let [module (string module-value)
                     prefix
                     (cond
                       (= "use" form-head) ""
                       as (string as "/")
                       (not (nil? explicit-prefix)) (string explicit-prefix)
                       (has-value? ["import" "import*"] form-head)
                       (string (first (string/split "." (path/basename module))) "/")
                       "")]]
        {:module module
         :kind form-head
         :alias (string/trimr prefix "/")
         :prefix prefix
         :only (if (indexed? only) (map string only) @[])
         :export (and (has-value? ["import" "import*"] form-head) exported)
         :top-level top-level
         :range (node-range form source line-starts)}))))

(defn- collect-nodes
  [node document-uri source mask line-starts definitions definitions-by-index
   references imports depth container]
  (when (dictionary? node)
    (def found
      (definition node document-uri source mask line-starts (= depth 0) container))
    (when found
      (array/push definitions found)
      (put definitions-by-index (node :index) found))
    (each imported (or (import-records node source mask line-starts (= depth 0)) @[])
      (array/push imports imported))
    (when (leaf? node)
      (array/push references
                   @{:name (node :value) :uri document-uri
                     :index (node :index)
                     :range (node-range node source line-starts)}))
    (when (indexed? (node :value))
      (each child (node :value)
        (collect-nodes child document-uri source mask line-starts
                       definitions definitions-by-index references imports
                       (if (form? node) (inc depth) depth)
                       (if found (found :identity) container))))))

(defn- same-position? [a b]
  (and a b (= (a :line) (b :line)) (= (a :character) (b :character))))

(defn- position-key [position]
  (string (position :line) ":" (position :character)))

(defn- local-identity [document-uri range]
  (string document-uri "#local:"
          (get-in range [:start :line]) ":"
          (get-in range [:start :character])))

(defn- definition-at-node [definitions node]
  (get definitions (node :index)))

(defn- callable-record [document-uri name identity range selection-range form local]
  {:name name
   :uri document-uri
   :identity identity
   :kind 12
   :form form
   :local local
   :range range
   :selection-range selection-range})

(defn- local-callables [node document-uri source line-starts]
  (def found @[])
  (def scope-range
    (and (number? (node :index)) (node-range node source line-starts)))
  (def direct-head
    (and (= :ptuple (node :tag)) (leaf? (get-in node [:value 0]))
         (get-in node [:value 0 :value])))
  (def [bindings expressions]
    (cond
      (has-value? [:let :loop] (node :tag))
      [(get-in (first (filter |(= :parameters ($ :tag)) (node :value)))
               [:value] @[])
       (get-in (first (filter |(= :expr ($ :tag)) (node :value)))
               [:value] @[])]

      (has-value? ["if-let" "when-let"] direct-head)
      (let [binding-form (get-in node [:value 1 :value] @[])]
        [(map |(get binding-form $) (range 0 (length binding-form) 2))
         (map |(get binding-form $) (range 1 (length binding-form) 2))])

      [@[] @[]]))
  (when (not (empty? bindings))
    # Destructuring flattens leaves, so only aligned simple bindings are safe.
    (when (= (length bindings) (length expressions))
      (eachp [index binding] bindings
        (def expression (get expressions index))
        (when (and (leaf? binding) (dictionary? expression)
                   (= :lambda (expression :tag)))
          (def selection-range (node-range binding source line-starts))
          (array/push
            found
            {:node-index (expression :index)
             :callable
             (merge
               (callable-record document-uri (binding :value)
                                (local-identity document-uri selection-range)
                                {:start (selection-range :start)
                                  :end (get-in (node-range expression source line-starts)
                                               [:end])}
                                selection-range
                                "fn" true)
               {:scope-range scope-range})})))))
  found)

(varfn collect-call-data [])

(varfn collect-call-data
  [node document-uri source line-starts definitions callables calls caller
   caller-overrides quoted qdepth]
  (when (dictionary? node)
    (def found (definition-at-node definitions node))
    (def form-head (and found (found :form)))
    (def executable (and (not quoted) (= 0 qdepth)))
    (var body-caller caller)
    (when (and executable found (get function-heads form-head))
      (array/push callables
                  (callable-record document-uri (found :name) (found :identity)
                                   (found :range) (found :selection-range)
                                   form-head (not (found :top-level))))
      (set body-caller (found :identity)))

    (def lambda-callers @{})
    (eachp [node-index identity] caller-overrides
      (put lambda-callers node-index identity))
    (when executable
      (each local (local-callables node document-uri source line-starts)
        (def callable (local :callable))
        (array/push callables callable)
        (put lambda-callers (local :node-index) (callable :identity))))

    # `(def name (fn ...))` is callable although its indexed symbol kind is a value.
    (when (and executable found (not (get function-heads form-head))
               (indexed? (node :value)))
      (when-let [lambda (first (filter |(and (dictionary? $) (= :lambda ($ :tag)))
                                       (node :value)))]
        (array/push callables
                    (callable-record document-uri (found :name) (found :identity)
                                     (found :range) (found :selection-range)
                                     form-head false))
        (put lambda-callers (lambda :index) (found :identity))))

    (def values (node :value))
    (cond
      (= :rmform (node :tag))
      (when-let [reader (get values 0)
                 child (first (filter dictionary? values))]
        (cond
          (= "'" reader)
           (collect-call-data child document-uri source line-starts
                              definitions callables calls
                              body-caller lambda-callers true qdepth)
          (= "~" reader)
           (collect-call-data child document-uri source line-starts
                              definitions callables calls
                              body-caller lambda-callers quoted (inc qdepth))
          (has-value? ["," ";"] reader)
          (when (> qdepth 0)
             (collect-call-data child document-uri source line-starts
                                definitions callables calls
                                body-caller lambda-callers quoted (dec qdepth)))
           (collect-call-data child document-uri source line-starts
                              definitions callables calls
                              body-caller lambda-callers quoted qdepth)))

      (indexed? values)
      (do
        (def direct-head
          (and (= :ptuple (node :tag))
               (dictionary? (get values 0))
               (leaf? (get values 0))
               (get-in values [0 :value])))
        (when (and direct-head (not found) (not quoted) (= 0 qdepth)
                   (not (has-value? ["quote" "quasiquote" "unquote" "splice"]
                                    direct-head)))
          (array/push calls @{:name direct-head
                              :uri document-uri
                              :caller body-caller
                              :range (node-range (values 0) source line-starts)}))
        (def child-quoted
          (or quoted
              (= "quote" direct-head)
              (and (= 0 qdepth) (has-value? ["unquote" "splice"] direct-head))))
        (def child-qdepth
          (cond
            (= "quasiquote" direct-head) (inc qdepth)
            (and (has-value? ["unquote" "splice"] direct-head) (> qdepth 0))
            (dec qdepth)
            qdepth))
        (each child values
          (when (dictionary? child)
            (collect-call-data
              child document-uri source line-starts definitions callables calls
              (or (get lambda-callers (child :index)) body-caller)
              lambda-callers child-quoted child-qdepth))))))
  [callables calls])

(defn analyze [document-uri content &opt syntax-tree]
  (def tree (or syntax-tree (parser/syntax-tree content)))
  (def mask (lookup/code-mask content))
  (def line-starts (lookup/line-starts content))
  (def definitions @[])
  (def definitions-by-index @{})
  (def references @[])
  (def imports @[])
  (try
    (each node (tree :value)
      (collect-nodes node document-uri content mask line-starts
                     definitions definitions-by-index references imports 0 nil))
    ([_] nil))
  (def bindings (parser/binding-ranges tree content line-starts))
  (def definitions-by-position @{})
  (each definition definitions
    (put definitions-by-position
         (position-key (get-in definition [:selection-range :start])) definition))
  (each reference references
    (def declared
      (get definitions-by-position (position-key (get-in reference [:range :start]))))
    (if declared
      (do
        (put reference :identity (declared :identity))
        (put reference :identity-kind :definition))
      (if-let [binding (parser/binding-at-index bindings (reference :name)
                                                (reference :index))]
        (do
          (put reference :identity
               (string document-uri "#local:"
                       (get-in binding [:start :line]) ":"
                       (get-in binding [:start :character])))
          (put reference :identity-kind :local))
        (when-let [resolved
                    (parser/binding-definition-at-index
                      bindings (reference :name) (reference :index))]
          (def definition
            (get definitions-by-position
                 (position-key (get-in resolved [:range :start]))))
          (put reference :identity
               (if definition
                 (definition :identity)
                 (string document-uri "#local:"
                         (get-in resolved [:range :start :line]) ":"
                         (get-in resolved [:range :start :character]))))
          (put reference :identity-kind (if definition :definition :local))))))
  (def callables @[])
  (def calls @[])
  (try
    (each node (tree :value)
      (collect-call-data node document-uri content line-starts definitions-by-index
                         callables calls nil @{} false 0))
    ([_] nil))
  (def references-by-position @{})
  (each reference references
    (put references-by-position (position-key (get-in reference [:range :start]))
         reference))
  (each call calls
    (when-let [reference (get references-by-position
                              (position-key (get-in call [:range :start])))]
      (put call :identity (reference :identity))))
  (each reference references (put reference :index nil))
  {:uri document-uri
   :content-hash (content-hash content)
   :definitions definitions
   :references references
   :imports imports
   :callables callables
   :calls calls})

(defn- module-candidates [document-uri module]
  (when-let [document-path (uri/file-uri->path document-uri)]
    (def base (path/abspath (path/join (path/dirname document-path) module)))
    [(uri/path->file-uri base)
     (uri/path->file-uri (string base ".janet"))
     (uri/path->file-uri (path/join base "init.janet"))]))

(defn module-uri [workspace document-uri module]
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

(defn module-uris [workspace document-uri module]
  (if (string/has-prefix? "." module)
    (filter |(get (workspace :index) $) (module-candidates document-uri module))
    (filter
      (fn [candidate]
        (when-let [candidate-path (uri/file-uri->path candidate)]
          (or (string/has-suffix? (string "/" module ".janet") candidate-path)
              (string/has-suffix? (string "/" module "/init.janet")
                                  candidate-path))))
      (keys (workspace :index)))))

(defn- definition-in [workspace document-uri name &opt definitions-by-document]
  (if definitions-by-document
    (get-in definitions-by-document [document-uri name])
    (first (filter |(and ($ :top-level) (= name ($ :name)))
                   (get-in workspace [:index document-uri :definitions] @[])))))

(varfn exported-definition [])

(varfn exported-definition [workspace document-uri name visited]
  (def key (string document-uri "#" name))
  (unless (get visited key)
    (put visited key true)
    (or (first (filter |(and ($ :top-level)
                             (= name ($ :name))
                             (not ($ :private))
                             (not (string/has-suffix? "-" ($ :form))))
                       (get-in workspace [:index document-uri :definitions] @[])))
        (some (fn [imported]
                (when (and (imported :export)
                           (imported :top-level)
                           (string/has-prefix? (imported :prefix) name))
                  (def target-name (string/slice name (length (imported :prefix))))
                  (when (or (empty? (imported :only))
                            (has-value? (imported :only) target-name))
                    (when-let [target-uri
                               (module-uri workspace document-uri (imported :module))]
                      (exported-definition workspace target-uri target-name visited)))))
               (get-in workspace [:index document-uri :imports] @[])))))

(varfn exported-definition-identities [])

(varfn exported-definition-identities [workspace document-uri name visited]
  (def key (string document-uri "#" name))
  (if (get visited key)
    @[]
    (do
      (put visited key true)
      (def identities
        (array
          ;(catseq [definition :in
                    (get-in workspace [:index document-uri :definitions] @[])
                    :when (and (definition :top-level)
                               (= name (definition :name))
                               (not (definition :private))
                               (not (string/has-suffix? "-" (definition :form))))]
             (definition :identity))))
      (each imported (get-in workspace [:index document-uri :imports] @[])
        (when (and (imported :export) (imported :top-level)
                   (string/has-prefix? (imported :prefix) name))
          (def target-name (string/slice name (length (imported :prefix))))
          (when (or (empty? (imported :only))
                    (has-value? (imported :only) target-name))
            (each target-uri
                  (module-uris workspace document-uri (imported :module))
              (each identity
                    (exported-definition-identities workspace target-uri target-name
                                                    visited)
                (array/push identities identity))))))
      (put visited key nil)
      (distinct identities))))

(varfn exported-definitions [])

(varfn exported-definitions [workspace document-uri &opt visited]
  (default visited @{})
  (if (get visited document-uri)
    @[]
    (do
      (put visited document-uri true)
      (def found
        (array
          ;(filter |(and ($ :top-level)
                         (not ($ :private))
                         (not (string/has-suffix? "-" ($ :form))))
                   (get-in workspace [:index document-uri :definitions] @[]))))
      (each imported (get-in workspace [:index document-uri :imports] @[])
        (when (and (imported :export) (imported :top-level))
          (when-let [target-uri
                     (module-uri workspace document-uri (imported :module))]
            (each definition (exported-definitions workspace target-uri visited)
              (when (or (empty? (imported :only))
                        (has-value? (imported :only) (definition :name)))
                (array/push
                  found
                  (merge definition
                         {:name (string (imported :prefix) (definition :name))})))))))
      (def seen @{})
      (def result
        (filter |(if (get seen ($ :name))
                   false
                   (do (put seen ($ :name) true) true))
                found))
      (put visited document-uri nil)
      result)))

(defn- position-before? [left right]
  (or (< (left :line) (right :line))
      (and (= (left :line) (right :line))
           (<= (left :character) (right :character)))))

(defn- visible-import? [imported position]
  (and (imported :top-level)
       (or (nil? position)
           (position-before? (get-in imported [:range :end]) position)
           (and (position-before? (get-in imported [:range :start]) position)
                (position-before? position (get-in imported [:range :end]))))))

(defn- range-contains? [outer inner]
  (and outer inner
       (position-before? (outer :start) (inner :start))
       (position-before? (inner :end) (outer :end))))

(defn- scoped-local-callable [callables name range]
  (last
    (sort-by |[(get-in $ [:selection-range :start :line])
               (get-in $ [:selection-range :start :character])]
             (filter |(and ($ :local)
                           (= name ($ :name))
                           ($ :scope-range)
                           (range-contains? ($ :scope-range) range)
                           (position-before? (get-in $ [:selection-range :start])
                                             (range :start)))
                     (get callables name @[])))))

(defn- module-cache-key [document-uri module]
  (string document-uri "\0" module))

(defn- exported-cache-key [document-uri name]
  (string document-uri "\0" name))

(defn- cached-exported-definition [workspace document-uri name cache]
  (if cache
    (let [key (exported-cache-key document-uri name)]
      (unless (has-key? cache key)
        (put cache key (exported-definition workspace document-uri name @{})))
      (get cache key))
    (exported-definition workspace document-uri name @{})))

(defn resolve-definition
  [workspace document-uri name &opt position definitions-by-document module-cache
   exported-cache]
  (def record (get (workspace :index) document-uri))
  (or (definition-in workspace document-uri name definitions-by-document)
      (some
        (fn [imported]
          (when (and (visible-import? imported position)
                     (has-value? ["import" "import*" "use"] (imported :kind))
                     (string/has-prefix? (imported :prefix) name))
            (def target-name (string/slice name (length (imported :prefix))))
            (when (and (not (empty? target-name))
                       (or (empty? (imported :only))
                           (has-value? (imported :only) target-name)))
              (when-let [target-uri
                         (if module-cache
                           (get module-cache
                                (module-cache-key document-uri (imported :module)))
                           (module-uri workspace document-uri (imported :module)))]
                (cached-exported-definition workspace target-uri target-name
                                            exported-cache)))))
        (record :imports))
      (when (> (length (string/split "/" name)) 1)
        (when
          (any?
            (map |(and (visible-import? $ position)
                       (has-value? ["import" "import*"] ($ :kind))
                       (not (empty? ($ :prefix)))
                       (string/has-prefix? ($ :prefix) name)
                        (nil? (if module-cache
                                (get module-cache
                                     (module-cache-key document-uri ($ :module)))
                                (module-uri workspace document-uri ($ :module)))))
                 (record :imports)))
          (first
            (filter |(not (or ($ :private)
                              (string/has-suffix? "-" ($ :form))))
                    (definitions workspace (last (string/split "/" name)))))))))

(defn relink [workspace]
  (def definitions-by-document @{})
  (def module-cache @{})
  (def exported-cache @{})
  (each record (values (workspace :index))
    (def by-name @{})
    (each definition (record :definitions)
      (when (and (definition :top-level) (nil? (get by-name (definition :name))))
        (put by-name (definition :name) definition)))
    (put definitions-by-document (record :uri) by-name)
    (each imported (get record :imports @[])
      (def key (module-cache-key (record :uri) (imported :module)))
      (unless (has-key? module-cache key)
        (put module-cache key
             (module-uri workspace (record :uri) (imported :module))))))
  (each record (values (workspace :index))
    (def local-callables @{})
    (each callable (get record :callables @[])
      (when (callable :local)
        (unless (get local-callables (callable :name))
          (put local-callables (callable :name) @[]))
        (array/push (get local-callables (callable :name)) callable)))
    (each reference (record :references)
      (when (= :import (reference :identity-kind))
        (put reference :identity nil)
        (put reference :identity-kind nil))
      (unless (reference :identity)
        (if-let [local (scoped-local-callable local-callables (reference :name)
                                               (reference :range))]
          (do
            (put reference :identity (local :identity))
            (put reference :identity-kind :local))
          (when-let [definition
                      (resolve-definition
                        workspace (record :uri) (reference :name)
                        (get-in reference [:range :start]) definitions-by-document
                        module-cache exported-cache)]
            (put reference :identity (definition :identity))
            (put reference :identity-kind :import))))))
  (def all-callables
    (catseq [record :in (values (workspace :index))
             callable :in (get record :callables @[])]
      callable))
  (def callables-by-identity @{})
  (def callable-counts @{})
  (def callable-name-counts @{})
  (each callable all-callables
    (put callables-by-identity (callable :identity) callable)
    (unless (callable :local)
      (def key (string (callable :uri) "\0" (callable :name)))
      (put callable-counts key (inc (get callable-counts key 0)))
      (put callable-name-counts (callable :name)
           (inc (get callable-name-counts (callable :name) 0)))))
  (each record (values (workspace :index))
    (def references-by-position @{})
    (each reference (record :references)
      (put references-by-position
           (position-key (get-in reference [:range :start])) reference))
    (each call (get record :calls @[])
      (put call :identity nil)
      (when-let [reference
                  (get references-by-position
                       (position-key (get-in call [:range :start])))
                  identity (reference :identity)
                  target (get callables-by-identity identity)]
        (def unresolved-import?
          (any? (map |(and (visible-import? $ (get-in call [:range :start]))
                           (has-value? ["import" "import*"] ($ :kind))
                           (not (empty? ($ :prefix)))
                           (string/has-prefix? ($ :prefix) (call :name))
                            (nil? (get module-cache
                                       (module-cache-key (record :uri) ($ :module)))))
                     (get record :imports @[]))))
        (def fallback-name (last (string/split "/" (call :name))))
        (def imported-identities
          (distinct
            (catseq [imported :in (get record :imports @[])
                     :when (and (visible-import? imported (get-in call [:range :start]))
                                (has-value? ["import" "import*" "use"]
                                            (imported :kind))
                                (string/has-prefix? (imported :prefix) (call :name)))
                     :let [target-name
                           (string/slice (call :name) (length (imported :prefix)))]
                     :when (and (not (empty? target-name))
                                (or (empty? (imported :only))
                                    (has-value? (imported :only) target-name)))
                      :let [target-uri
                            (get module-cache
                                 (module-cache-key (record :uri) (imported :module)))
                            definition
                            (and target-uri
                                 (cached-exported-definition
                                   workspace target-uri target-name exported-cache))]
                     :when definition]
              (definition :identity))))
        (def duplicate?
          (or (and (not (target :local))
                    (> (get callable-counts
                            (string (target :uri) "\0" (target :name)) 0)
                       1))
              (and unresolved-import?
                    (not= 1 (get callable-name-counts fallback-name 0)))
              (> (length imported-identities) 1)))
        (unless duplicate? (put call :identity identity)))))
  workspace)

(defn update [workspace document-uri content]
  (put (workspace :index) document-uri (analyze document-uri content))
  (relink workspace))

(defn add-generated-to-record [record document-uri document-path env]
  (when (and record document-path)
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
             :private (not (not (binding :private)))
             :type-target nil
             :implementation-targets @[]
             :top-level true
             :range {:start location :end location}
             :selection-range {:start location :end location}
             :children @[]})))))
  record)

(defn update-record [workspace document-uri record]
  (put (workspace :index) document-uri record)
  (relink workspace))

(defn add-generated [workspace document-uri env]
  (when-let [record (get (workspace :index) document-uri)]
    (add-generated-to-record record document-uri
                             (uri/file-uri->path document-uri) env))
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

(defn definition-by-identity [workspace identity]
  (first
    (catseq [record :in (values (workspace :index))
             definition :in (record :definitions)
             :when (= identity (definition :identity))]
      definition)))

(defn- metadata-target-identity [workspace definition target]
  (when target
    (when-let [record (get-in workspace [:index (definition :uri)])]
      (def same-file-identities
        (distinct
          (catseq [candidate :in (record :definitions)
                   :when (and (candidate :top-level)
                              (= (target :name) (candidate :name)))]
            (candidate :identity))))
      (def imported-identities
        (distinct
          (catseq [imported :in (record :imports)
                   :when (and (visible-import? imported (get-in target [:range :start]))
                              (has-value? ["import" "import*" "use"]
                                          (imported :kind))
                              (string/has-prefix? (imported :prefix) (target :name)))
                   :let [target-name
                         (string/slice (target :name) (length (imported :prefix)))]
                   :when (and (not (empty? target-name))
                              (or (empty? (imported :only))
                                  (has-value? (imported :only) target-name)))
                   target-uri :in
                   (module-uris workspace (definition :uri) (imported :module))
                   identity :in
                   (exported-definition-identities workspace target-uri target-name @{})]
            identity)))
      (def ambiguous-module?
        (any?
          (map |(and (visible-import? $ (get-in target [:range :start]))
                     (has-value? ["import" "import*" "use"] ($ :kind))
                     (string/has-prefix? ($ :prefix) (target :name))
                     (not= 1 (length (module-uris workspace (definition :uri)
                                                 ($ :module)))))
               (record :imports))))
      (cond
        ambiguous-module? nil
        (= 1 (length same-file-identities)) (same-file-identities 0)
        (> (length same-file-identities) 1) nil
        (= 1 (length imported-identities)) (imported-identities 0)
        nil))))

(defn type-definition [workspace identity]
  (when-let [definition (definition-by-identity workspace identity)
             target-identity
             (metadata-target-identity workspace definition
                                       (definition :type-target))]
    (definition-by-identity workspace target-identity)))

(defn implementations [workspace identity]
  (sort-by |[($ :uri)
             (get-in $ [:selection-range :start :line])
             (get-in $ [:selection-range :start :character])]
           (distinct
             (catseq [record :in (values (workspace :index))
                      definition :in (record :definitions)
                      target :in (get definition :implementation-targets @[])
                      :let [target-identity
                            (metadata-target-identity workspace definition target)]
                      :when (= identity target-identity)]
               definition))))

(defn callables [workspace]
  (catseq [record :in (values (workspace :index))
           callable :in (get record :callables @[])]
    callable))

(defn callable-by-identity [workspace identity]
  (first (filter |(= identity ($ :identity)) (callables workspace))))

(defn incoming-calls [workspace identity]
  (catseq [record :in (values (workspace :index))
           call :in (get record :calls @[])
           :when (= identity (call :identity))]
    call))

(defn outgoing-calls [workspace identity]
  (catseq [record :in (values (workspace :index))
           call :in (get record :calls @[])
           :when (and (= identity (call :caller)) (call :identity))]
    call))

(defn- excluded-relative-path? [relative exclusions]
  (def parts (string/split "/" relative))
  (or (any? (map |(has-value? exclusions $) parts))
      (any? (map |(string/has-prefix? "." $)
                 (array/slice parts 0 (max 0 (dec (length parts))))))))

(defn- git-files [root]
  (try
    (with [proc (os/spawn ["git" "-C" root "ls-files" "--cached" "--others"
                           "--exclude-standard" "-z"] :xp {:out :pipe :err :pipe})]
      (let [[out _ status] (ev/gather
                             (ev/read (proc :out) :all)
                             (ev/read (proc :err) :all)
                             (os/proc-wait proc))]
        (and (= 0 status) out
             (filter |(not (empty? $)) (string/split "\0" out)))))
    ([_] nil)))

(defn source-files [root exclusions suffixes]
  (unless (os/stat root)
    (error (string "workspace root does not exist: " root)))
  (def found @[])
  (if-let [tracked (git-files root)]
    (each relative tracked
      (def current (path/join root relative))
      (when (and (= :file (os/stat current :mode))
                 (not (excluded-relative-path? relative exclusions))
                 (any? (map |(string/has-suffix? $ current) suffixes)))
        (array/push found current)))
    (let [pending @[[root ""]]]
      (while (not (empty? pending))
        (def [current relative] (array/pop pending))
        (case (os/stat current :mode)
          :directory
          (unless (and (not (empty? relative))
                       (excluded-relative-path? (string relative "/file") exclusions))
            (each entry (os/dir current)
              (array/push pending
                          [(path/join current entry)
                           (if (empty? relative) entry (path/join relative entry))])))
          :file
          (when (and (not (excluded-relative-path? relative exclusions))
                     (any? (map |(string/has-suffix? $ current) suffixes)))
            (array/push found current))))))
  (sort found))

(defn files [root exclusions]
  (source-files root exclusions [".janet"]))

(defn scan [root exclusions]
  (def records @{})
  (each current (files root exclusions)
    (try
      (do
        (def document-uri (uri/path->file-uri current))
        (put records document-uri (analyze document-uri (slurp current))))
      ([_] nil)))
  (def workspace @{:index records})
  (relink workspace)
  (workspace :index))
