(def function-heads ['defn 'defn- 'varfn 'varfn- 'fn])
(def special-call-heads
  '[break def do fn if quasiquote quote set splice unquote upscope var while])

(defn- parse-forms [source]
  (def state (parser/new))
  (parser/consume state source)
  (parser/eof state)
  (def forms @[])
  (while (parser/has-more state)
    (array/push forms ((parser/produce state true) 0)))
  forms)

(defn- parameter-list [form]
  (first (filter |(and (tuple? $) (= :brackets (tuple/type $))) form)))

(varfn collect-parameters [])

(varfn collect-parameters [value names]
  (cond
    (symbol? value)
    (when (and (not (string/has-prefix? "&" value))
               (not (string/has-prefix? "_" value)))
      (array/push names value))
    (indexed? value) (each child value (collect-parameters child names))
    (dictionary? value) (each child (values value) (collect-parameters child names)))
  names)

(defn- parameter-names [parameters]
  (distinct (collect-parameters parameters @[])))

(defn- symbol-used? [node name]
  (cond
    (symbol? node) (= node name)
    (indexed? node) (any? (map |(symbol-used? $ name) node))
    (dictionary? node) (or (any? (map |(symbol-used? $ name) (keys node)))
                           (any? (map |(symbol-used? $ name) (values node))))
    false))

(defn- function-form? [form]
  (and (tuple? form) (not (empty? form)) (has-value? function-heads (form 0))))

(defn- call-kinds [forms env]
  (def macros @{})
  (def functions @{})
  (each name (all-bindings env)
    (def binding (get env name))
    (when (and (dictionary? binding) (binding :macro))
      (put macros name true)))
  (walk (fn [form]
          (when (and (tuple? form) (>= (length form) 2)
                     (symbol? (form 1)))
            (cond
              (has-value? ['defmacro 'defmacro-] (form 0))
              (put macros (form 1) true)
              (has-value? ['defn 'defn- 'varfn 'varfn-] (form 0))
              (put functions (form 1) true)))
          form)
        forms)
  [macros functions])

(defn- uncertain-call? [node macros functions env]
  (and (indexed? node)
       (not (empty? node))
       (or (and (tuple? node)
                (symbol? (node 0))
                (or (get macros (node 0))
                    (and (not (get functions (node 0)))
                         (not (has-value? special-call-heads (node 0)))
                         (nil? (get env (node 0))))))
           (and (not (and (tuple? node)
                          (has-value? ['quote 'quasiquote] (node 0))))
                (any? (map |(uncertain-call? $ macros functions env) node))))))

(defn- unused-parameters [form macros functions env]
  (def parameters (parameter-list form))
  (if (nil? parameters)
    @[]
    (let [parameter-index (find-index |(= parameters $) form)
          body (tuple/slice form (inc parameter-index))
          [line column] (tuple/sourcemap form)]
      (if (any? (map |(uncertain-call? $ macros functions env) body))
        @[]
        (catseq [name :in (parameter-names parameters)
                 :when (not (symbol-used? body name))]
          {:message (string "unused parameter " name)
           :location [line column]
           :severity 2
           :code "janet.lint.unused-parameter"
           :data {:name (string name)}})))))

(defn analyze [source &opt env]
  (try
    (let [diagnostics @[]
          forms (parse-forms source)
          env (or env root-env)
          [macros functions] (call-kinds forms env)]
      (walk (fn [form]
              (when (function-form? form)
                (array/concat diagnostics
                              (unused-parameters form macros functions env)))
              form)
            forms)
      diagnostics)
    ([_] @[])))
