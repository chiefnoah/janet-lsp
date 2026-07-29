(import ./index)
(import ./logging)
(import ./lookup)

(def special-symbols
  {"break" true "def" true "def-" true "defglobal" true
   "defmacro" true "defmacro-" true "defn" true "defn-" true
   "do" true "false" true "fn" true "if" true "nil" true
   "quasiquote" true "quote" true "set" true "splice" true
   "true" true "unquote" true "upscope" true "var" true
   "var-" true "varglobal" true "varfn" true "varfn-" true
   "while" true})

(def function-forms
  {"defn" true "defn-" true "varfn" true "varfn-" true})
(def macro-forms {"defmacro" true "defmacro-" true})

(defn- copy-set [values]
  (def copied @{})
  (eachp [key value] values (when value (put copied key true)))
  copied)

(defn- parse-forms [source]
  (def state (parser/new))
  (parser/consume state source)
  (parser/eof state)
  (def forms @[])
  (while (parser/has-more state)
    (array/push forms ((parser/produce state true) 0)))
  forms)

(defn- node-range [node source]
  {:start (lookup/from-index (node :index) source)
   :end (lookup/from-index (+ (node :index) (node :len)) source)})

(defn- leaf? [node]
  (and (dictionary? node) (string? (node :value))))

(defn- leaves [node &opt found]
  (default found @[])
  (cond
    (leaf? node) (array/push found node)
    (and (dictionary? node) (indexed? (node :value)))
    (each child (node :value) (leaves child found)))
  found)

(defn- binding-container? [node]
  (and (dictionary? node)
       (has-value? [:parameters :variables] (node :tag))))

(defn- binding-nodes [node]
  (if (binding-container? node) (leaves node) @[]))

(defn- binding-key [node source]
  (let [range (node-range node source)]
    (string (node :value) "@" (get-in range [:start :line]) ":"
            (get-in range [:start :character]))))

(defn- binding-diagnostics [tree source]
  (def results @[])
  (def declarations @{})
  (defn introduce [binding outer local introduced]
    (def name (binding :value))
    (put declarations (binding-key binding source) true)
    (when (and (not (string/has-prefix? "&" name))
               (not (string/has-prefix? "_" name))
               (not (string/has-prefix? ":" name)))
      (when (or (get outer name) (get introduced name))
        (def range (node-range binding source))
        (array/push results
                    {:message (string "binding " name " shadows an existing binding")
                     :range range
                     :location [(inc (get-in range [:start :line]))
                                (inc (get-in range [:start :character]))]
                     :severity 2
                     :code "janet.lint.shadowing"}))
      (put introduced name true)
      (put local name true)))
  (defn visit [node outer]
    (when (dictionary? node)
      (when (indexed? (node :value))
        (let [children (node :value)
              binders (flatten (map binding-nodes children))
              local (copy-set outer)
              introduced @{}]
          (cond
            (= :loop (node :tag)) nil

            (= :let (node :tag))
            (do
              (def parameters
                (get-in (first (filter |(= :parameters (get $ :tag)) children))
                        [:value] @[]))
              (def expressions
                (get-in (first (filter |(= :expr (get $ :tag)) children))
                        [:value] @[]))
              (var parameter-index 0)
              (each expression expressions
                (visit expression local)
                (while (and (< parameter-index (length parameters))
                            (< (get-in parameters [parameter-index :index])
                               (expression :index)))
                  (introduce (parameters parameter-index)
                             outer local introduced)
                  (+= parameter-index 1)))
              (while (< parameter-index (length parameters))
                (introduce (parameters parameter-index) outer local introduced)
                (+= parameter-index 1))
              (each child children
                (unless (has-value? [:parameters :expr] (get child :tag))
                  (visit child local))))

            (has-value? [:defn :lambda] (node :tag))
            (do
              (each binding binders (introduce binding outer local introduced))
              (each child children
                (unless (= :parameters (get child :tag)) (visit child local))))

            (has-value? [:def :for-each] (node :tag))
            (do
              (each binding binders (introduce binding outer local introduced))
              (each child children
                (unless (binding-container? child) (visit child outer))))

            (each child children (visit child outer)))))))
  (visit tree @{})
  [results declarations])

(defn- position-key [name range]
  (string name "@" (get-in range [:start :line]) ":"
          (get-in range [:start :character])))

(defn- in-range? [position range]
  (let [start (range :start) end (range :end)]
    (and (or (> (position :line) (start :line))
             (and (= (position :line) (start :line))
                  (>= (position :character) (start :character))))
         (or (< (position :line) (end :line))
             (and (= (position :line) (end :line))
                  (<= (position :character) (end :character)))))))

(defn- before? [left right]
  (or (< (left :line) (right :line))
      (and (= (left :line) (right :line))
           (< (left :character) (right :character)))))

(defn- local-definition [record name &opt position]
  (first
    (filter |(and ($ :top-level)
                  (= name ($ :name))
                  (or (nil? position)
                      (before? (get-in $ [:selection-range :start]) position)))
            (record :definitions))))

(defn- opaque-call? [name record env &opt position]
  (let [definition (local-definition record name position)
        binding (get env (symbol name))]
    (or (and definition (get macro-forms (definition :form)))
        (and (dictionary? binding) (binding :macro))
        (and (not (get special-symbols name))
             (not (and definition (get function-forms (definition :form))))
             (nil? binding)))))

(defn- ignored-ranges [tree source imports record env]
  (def ranges (map |($ :range) imports))
  (defn visit [node quoted]
    (when (dictionary? node)
      (def children (and (indexed? (node :value)) (node :value)))
      (def first-child (and children (first children)))
      (def first-leaf (and (leaf? first-child) first-child))
      (def quote-form
        (and first-leaf (has-value? ["quote" "quasiquote"] (first-leaf :value))))
      (when (or quoted quote-form (= :rmform (node :tag)) (= :loop (node :tag)))
        (array/push ranges (node-range node source)))
      (when (and children
                 (not (or quoted quote-form (= :rmform (node :tag))
                          (= :loop (node :tag)))))
        (if (and (= :ptuple (node :tag)) first-leaf
                 (opaque-call? (first-leaf :value) record env
                               (get-in (node-range first-leaf source) [:start])))
          (each child (array/slice children 1)
            (array/push ranges (node-range child source)))
          (each child children (visit child false))))))
  (visit tree false)
  ranges)

(defn- resolution [workspace record name position]
  (if (local-definition record name position)
    :defined
    (do
      (var matched false)
      (var unknown false)
      (var defined false)
      (each imported (record :imports)
        (when (and (imported :top-level)
                   (before? (get-in imported [:range :end]) position)
                   (has-value? ["import" "import*" "use"] (imported :kind))
                   (string/has-prefix? (imported :prefix) name))
          (def target-name (string/slice name (length (imported :prefix))))
          (when (and (not (empty? target-name))
                     (or (empty? (imported :only))
                         (has-value? (imported :only) target-name)))
            (set matched true)
            (if-let [target-uri
                     (index/module-uri workspace (record :uri) (imported :module))]
              (when (index/exported-definition workspace target-uri target-name @{})
                (set defined true))
              (set unknown true)))))
      (cond defined :defined unknown :unknown matched :undefined :undefined))))

(defn- undefined-diagnostics [source record workspace env declarations ignored]
  (def overlay @{})
  (eachp [document-uri indexed-record] (workspace :index)
    (put overlay document-uri indexed-record))
  (put overlay (record :uri) record)
  (def resolving (merge workspace {:index overlay}))
  (catseq [reference :in (record :references)
           :let [name (reference :name)
                 range (reference :range)
                 position (range :start)]
           :when (and (not (reference :identity))
                      (not (scan-number name))
                      (not (string/has-prefix? ":" name))
                      (not (get special-symbols name))
                      (not (get env (symbol name)))
                      (not (get declarations (position-key name range)))
                      (not (any? (map |(in-range? position $) ignored)))
                      (= :undefined (resolution resolving record name position)))]
    {:message (string "undefined symbol " name)
     :range range
     :location [(inc (position :line)) (inc (position :character))]
     :severity 1
     :code "janet.lint.undefined-symbol"
     :data {:name name}}))

(defn- duplicate-diagnostics [record]
  (def seen @{})
  (def results @[])
  (each definition (record :definitions)
    (when (definition :top-level)
      (def name (definition :name))
      (if (get seen name)
        (let [position (get-in definition [:selection-range :start])]
          (array/push results
                      {:message (string "duplicate definition " name)
                       :range (definition :selection-range)
                       :location [(inc (position :line)) (inc (position :character))]
                       :severity 2
                       :code "janet.lint.duplicate-definition"}))
        (put seen name true))))
  results)

(defn- unused-binding-diagnostics [tree source record env]
  (def results @[])
  (defn uncertain? [node]
    (when (dictionary? node)
      (def children (and (indexed? (node :value)) (node :value)))
      (or (and (= :ptuple (node :tag))
               (leaf? (get children 0))
               (opaque-call? (get-in children [0 :value]) record env
                             (get-in (node-range (get children 0) source) [:start])))
          (and children (any? (map uncertain? children))))))
  (defn visit [node]
    (when (dictionary? node)
      (when (and (= :let (node :tag)) (not (uncertain? node)))
        (when-let [parameters
                   (first (filter |(= :parameters (get $ :tag)) (node :value)))]
          (each binding (parameters :value)
            (def name (binding :value))
            (when (and (leaf? binding)
                       (not (string/has-prefix? "_" name))
                       (not (string/has-prefix? "&" name))
                       (not (string/has-prefix? ":" name)))
              (def range (node-range binding source))
              (def identity
                (string (record :uri) "#local:"
                        (get-in range [:start :line]) ":"
                        (get-in range [:start :character])))
              (when (<= (length (filter |(= identity ($ :identity))
                                        (record :references)))
                        1)
                (array/push results
                            {:message (string "unused binding " name)
                             :range range :severity 2
                             :code "janet.lint.unused-binding"
                             :data {:name name}}))))))
      (when (indexed? (node :value))
        (each child (node :value) (visit child)))))
  (visit tree)
  results)

(defn- unused-import-diagnostics [record workspace]
  (def results @[])
  (each imported (record :imports)
    # Multi-module `use` forms need per-module rewriting, so remain untouched.
    (when (and (imported :top-level) (= "import" (imported :kind))
               (not (imported :export)) (not (empty? (imported :prefix)))
               (not (string/has-prefix? "_" (imported :alias))))
      (when-let [target-uri
                 (index/module-uri workspace (record :uri) (imported :module))]
        (def exported
          (filter |(and (or (empty? (imported :only))
                            (has-value? (imported :only) ($ :name)))
                        (not ($ :private)))
                  (index/exported-definitions workspace target-uri)))
        (def used
          (any?
            (map |(and (not (in-range? (get-in $ [:range :start])
                                       (imported :range)))
                       (string/has-prefix? (imported :prefix) ($ :name)))
                 (record :references))))
        (when (and (not (empty? exported)) (not used))
          (array/push results
                      {:message (string "unused import " (imported :module))
                       :range (imported :range) :severity 2
                       :code "janet.lint.unused-import"
                       :data {:module (imported :module)
                              :alias (imported :alias)
                              :prefix (imported :prefix)}})))))
  results)

(defn- literal-condition [value]
  (cond
    (or (nil? value) (= false value)) :false
    (or (= true value) (number? value) (string? value) (keyword? value)
        (array? value) (table? value) (struct? value)) :true
    nil))

(defn- source-location [form &opt fallback]
  (if (tuple? form)
    (tuple/sourcemap form)
    (or fallback [1 1])))

(defn- control-flow-diagnostics [forms record env]
  (def results @[])
  (var visit nil)
  (defn warn [form fallback code message severity]
    (array/push results {:message message :location (source-location form fallback)
                         :severity severity :code code}))
  (defn visit-sequence [sequence fallback]
    (var terminated false)
    (each form sequence
      (if terminated
        (warn form fallback "janet.lint.unreachable-code" "unreachable code" 2)
        (do
          (visit form)
          (when (and (tuple? form) (not (empty? form)) (= 'break (form 0)))
            (set terminated true))))))
  (set visit (fn visit [form]
    (when (tuple? form)
      (def fallback (tuple/sourcemap form))
      (def head (and (not (empty? form)) (form 0)))
      (case head
        'if
        (when (> (length form) 1)
          (when-let [condition (literal-condition (form 1))]
            (warn (form 1) fallback "janet.lint.constant-condition"
                  (string "condition is always " (if (= :true condition) "true" "false")) 3)
            (when (and (= :false condition) (> (length form) 2))
              (warn (form 2) fallback "janet.lint.unreachable-code" "unreachable branch" 2))
            (when (and (= :true condition) (> (length form) 3))
              (warn (form 3) fallback "janet.lint.unreachable-code" "unreachable branch" 2)))
          (each child (tuple/slice form 1) (visit child)))

        'while
        (when (> (length form) 1)
          (when-let [condition (literal-condition (form 1))]
            (warn (form 1) fallback "janet.lint.constant-condition"
                  (string "condition is always " (if (= :true condition) "true" "false")) 3)
            (when (= :false condition)
              (each child (tuple/slice form 2)
                (warn child fallback "janet.lint.unreachable-code" "unreachable loop body" 2))))
          (each child (tuple/slice form 1) (visit child)))

        'do (visit-sequence (tuple/slice form 1) fallback)

        'quote nil
        'quasiquote nil

        (if (has-value? ['defn 'defn- 'defmacro 'defmacro- 'varfn 'varfn- 'fn] head)
          (when-let [parameters
                     (find-index |(and (tuple? $) (= :brackets (tuple/type $))) form)]
            (visit-sequence (tuple/slice form (inc parameters)) fallback))
          (unless (and (symbol? head)
                       (opaque-call? (string head) record env
                                     {:line (max 0 (dec (fallback 0)))
                                      :character (max 0 (dec (fallback 1)))}))
            (each child form (visit child))))))))
  (each form forms (visit form))
  results)

(defn analyze [source tree record workspace env &opt include-undefined]
  (default include-undefined true)
  (try
    (let [[shadowing declarations] (binding-diagnostics tree source)
          ignored (ignored-ranges tree source (record :imports) record env)]
      (array ;(if include-undefined
                (undefined-diagnostics source record workspace env declarations ignored)
                @[])
              ;(duplicate-diagnostics record)
              ;(unused-binding-diagnostics tree source record env)
              ;(unused-import-diagnostics record workspace)
              ;shadowing
             ;(control-flow-diagnostics (parse-forms source) record env)))
    ([err]
      (logging/warn (string "Static diagnostics failed: " err) [:diagnostics])
      @[])))
