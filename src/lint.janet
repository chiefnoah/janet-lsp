(def function-heads ['defn 'defn- 'varfn 'varfn- 'fn])

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

(defn- parameter-names [parameters]
  (def names @[])
  (walk (fn [value]
          (when (and (symbol? value)
                     (not (string/has-prefix? "&" value))
                     (not (string/has-prefix? "_" value)))
            (array/push names value))
          value)
        parameters)
  (distinct names))

(defn- symbol-used? [node name]
  (cond
    (symbol? node) (= node name)
    (indexed? node) (any? (map |(symbol-used? $ name) node))
    (dictionary? node) (or (any? (map |(symbol-used? $ name) (keys node)))
                           (any? (map |(symbol-used? $ name) (values node))))
    false))

(defn- function-form? [form]
  (and (tuple? form) (not (empty? form)) (has-value? function-heads (form 0))))

(defn- unused-parameters [form]
  (def parameters (parameter-list form))
  (if (nil? parameters)
    @[]
    (let [parameter-index (find-index |(= parameters $) form)
          body (tuple/slice form (inc parameter-index))
          [line column] (tuple/sourcemap form)]
      (catseq [name :in (parameter-names parameters)
               :when (not (symbol-used? body name))]
        {:message (string "unused parameter " name)
         :location [line column]
         :severity 2
         :code "janet.lint.unused-parameter"}))))

(defn analyze [source]
  (try
    (let [diagnostics @[]]
      (walk (fn [form]
              (when (function-form? form)
                (array/concat diagnostics (unused-parameters form)))
              form)
            (parse-forms source))
      diagnostics)
    ([_] @[])))
