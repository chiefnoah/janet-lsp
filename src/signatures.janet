(import ./lookup)
(import ./parser)

(def function-heads ['defn 'defn- 'varfn 'varfn-])

(defn- parse-forms [source]
  (def state (parser/new))
  (parser/consume state source)
  (parser/eof state)
  (def forms @[])
  (while (parser/has-more state)
    (array/push forms ((parser/produce state true) 0)))
  forms)

(defn- parameter-list [form]
  (first (filter |(and (tuple? $) (= :brackets (tuple/type $)))
                 (tuple/slice form 2))))

(defn- parameter-label [parameter]
  (if (symbol? parameter)
    (string parameter)
    (string/format "%q" parameter)))

(defn- parameters [parameter-list]
  (def result @[])
  (var kind :required)
  (each parameter parameter-list
    (if (and (symbol? parameter)
             (has-value? ['&opt '& '&named] parameter))
      (set kind (case parameter '&opt :optional '& :rest '&named :named))
      (do
        (def label (parameter-label parameter))
        (array/push result
                    {:label (if (= kind :named) (string ":" label) label)
                     :name label
                     :kind kind})
        (when (= kind :rest) (set kind :after-rest)))))
  result)

(defn- token-range [source form token-index]
  (def [line column] (tuple/sourcemap form))
  (def form-start
    (lookup/to-index {:line (max 0 (dec line)) :character (max 0 (dec column))}
                     source))
  (def tokens
    (filter |(not (empty? ($ 1)))
            (or (peg/match lookup/word-peg
                           (string/slice (lookup/code-mask source) form-start))
                @[])))
  (when (> (length tokens) token-index)
    (def token (tokens token-index))
    {:start (lookup/from-index (+ form-start (token 0)) source)
     :end (lookup/from-index (+ form-start (token 2)) source)}))

(defn- make-signature [source form]
  (when (and (tuple? form) (>= (length form) 3)
             (has-value? function-heads (form 0))
             (symbol? (form 1)))
    (when-let [raw-parameters (parameter-list form)]
      (def parsed-parameters (parameters raw-parameters))
      (def positional
        (filter |(has-value? [:required :optional] ($ :kind)) parsed-parameters))
      (def named (filter |(= :named ($ :kind)) parsed-parameters))
      (def rendered (string/format "%q" raw-parameters))
      {:name (string (form 1))
       :definition-range (token-range source form 1)
       :label (string "(" (form 1)
                      (if (empty? raw-parameters) "" " ")
                      (string/slice rendered 1 (dec (length rendered))) ")")
       :parameters parsed-parameters
       :required (length (filter |(= :required ($ :kind)) parsed-parameters))
       :positional (length positional)
       :variadic (any? (map |(= :rest ($ :kind)) parsed-parameters))
       :named named})))

(defn all [source]
  (try
    (catseq [form :in (parse-forms source)
             :let [signature (make-signature source form)]
             :when signature]
      signature)
    ([_] @[])))

(defn find [source name]
  (def matches (filter |(= name ($ :name)) (all source)))
  (when (= 1 (length matches)) (matches 0)))

(defn find-in [all-signatures name]
  (def matches (filter |(= name ($ :name)) all-signatures))
  (when (= 1 (length matches)) (matches 0)))

(defn- diagnostic [code severity message form &opt data]
  (def [line column] (tuple/sourcemap form))
  {:code code :severity severity :message message :location [line column]
   :data (merge {:callee (string (form 0))} (or data {}))})

(defn- dynamic-arguments? [arguments]
  (any? (map |(and (tuple? $) (not (empty? $))
                   (has-value? ['splice 'unquote] ($ 0)))
             arguments)))

(defn- validate-call [source form signature]
  (def arguments (tuple/slice form 1))
  (if (dynamic-arguments? arguments)
    @[]
    (let [result @[]
          argument-count (length arguments)
          positional-count (min argument-count (signature :positional))]
      (when (< positional-count (signature :required))
        (array/push result
                    (diagnostic
                      "janet.call.missing-arguments" 1
                      (string (signature :name) " expects at least "
                              (signature :required) " positional arguments, got "
                              positional-count)
                      form
                      {:missing (- (signature :required) positional-count)
                       :provided positional-count})))
      (cond
        (signature :variadic) nil

        (not (empty? (signature :named)))
        (let [named-arguments
              (array/slice arguments (min argument-count (signature :positional)))
              known @{}
              seen @{}]
          (each parameter (signature :named)
            (put known (parameter :label) true))
          (when (odd? (length named-arguments))
            (array/push result
                        (diagnostic "janet.call.odd-named-arguments" 2
                                    (string (signature :name)
                                            " expects named arguments as keyword/value pairs")
                                    form)))
          (var index 0)
          (while (< index (dec (length named-arguments)))
            (def key (named-arguments index))
            (def label (if (keyword? key) (string ":" key) (string key)))
            (if (not (get known label))
              (array/push result
                          (diagnostic "janet.call.unknown-named-argument" 2
                                      (string "unknown named argument " label " for "
                                              (signature :name))
                                      form {:label label
                                            :positional (signature :positional)
                                            :named-index index}))
              (if (get seen label)
                (array/push result
                            (diagnostic "janet.call.duplicate-named-argument" 2
                                        (string "duplicate named argument " label " for "
                                                (signature :name))
                                        form))
                (put seen label true)))
            (+= index 2)))

        (> argument-count (signature :positional))
        (array/push result
                    (diagnostic
                      "janet.call.extra-arguments" 1
                      (string (signature :name) " expects at most "
                              (signature :positional) " positional arguments, got "
                              argument-count)
                      form)))
      result)))

(defn- matching-definition? [source form signature &opt call]
  (if call
    (and (signature :identity) (call :identity)
         (= (signature :identity) (call :identity)))
    (when-let [call-range (token-range source form 0)
               resolved (parser/definition-at (call-range :start)
                                              source (signature :name))]
      (deep= (get-in resolved [:range :start])
             (get-in signature [:definition-range :start])))))

(varfn validate-node [])

(varfn validate-node [source node signatures diagnostics &opt calls-by-position]
  (cond
    (tuple? node)
    (unless (and (not (empty? node)) (has-value? ['quote 'quasiquote] (node 0)))
      (when (and (not (empty? node)) (symbol? (node 0)))
        (def name (string (node 0)))
        (def [line column] (tuple/sourcemap node))
        (def call
          (and calls-by-position
               (get calls-by-position
                    (string name ":" (max 0 (dec line)) ":"
                            (max 0 column)))))
        (when-let [matches (get signatures name)]
          (when (and (= 1 (length matches))
                     (matching-definition? source node (matches 0) call))
            (array/concat diagnostics (validate-call source node (matches 0))))))
      (each child node
        (validate-node source child signatures diagnostics calls-by-position)))
    (indexed? node)
    (each child node
      (validate-node source child signatures diagnostics calls-by-position))
    (dictionary? node)
    (each child (values node)
      (validate-node source child signatures diagnostics calls-by-position)))
  diagnostics)

(defn diagnostics [source &opt record]
  (try
    (let [by-name @{}
          forms (parse-forms source)
          diagnostics @[]
          definitions-by-name @{}
          calls-by-position (and record @{})]
      (each definition (get record :definitions @[])
        (unless (get definitions-by-name (definition :name))
          (put definitions-by-name (definition :name) @[]))
        (array/push (get definitions-by-name (definition :name)) definition))
      (each call (get record :calls @[])
        (put calls-by-position
             (string (call :name) ":" (get-in call [:range :start :line]) ":"
                     (get-in call [:range :start :character]))
             call))
      (each form forms
        (when-let [signature (make-signature source form)]
          (def definition
            (first
              (filter |(deep= ($ :selection-range) (signature :definition-range))
                      (get definitions-by-name (signature :name) @[]))))
          (def resolved
            (if definition
              (merge signature {:identity (definition :identity)})
              signature))
          (unless (get by-name (resolved :name))
            (put by-name (resolved :name) @[]))
          (array/push (get by-name (resolved :name)) resolved)))
      (each form forms
        (validate-node source form by-name diagnostics calls-by-position))
      diagnostics)
    ([_] @[])))

(defn used-named-arguments [source context signature]
  (try
    (let [fragment (string/slice source
                                 (first (context :range)) (last (context :range)))
          call (parse (string "(" fragment ")"))
          arguments (array/slice call
                                 (min (length call) (inc (signature :positional))))
          names @[]]
      (var index 0)
      (while (< index (length arguments))
        (when (keyword? (arguments index))
          (array/push names (string ":" (arguments index))))
        (+= index 2))
      (distinct names))
    ([_] @[])))

(defn active-parameter [signature context source]
  (def used (used-named-arguments source context signature))
  (def named-label (last used))
  (def named-index
    (and named-label
         (find-index |(= named-label ($ :label)) (signature :parameters))))
  (or named-index
      (min (context :active-parameter)
           (max 0 (dec (length (signature :parameters)))))))
