(import ./doc)
(import ./index)
(import ./lookup)
(import ./position)
(import ./server-utils)
(import ./signatures)

(def non-call-heads
  {"def" true "def-" true "defglobal" true "defmacro" true "defmacro-" true
   "defn" true "defn-" true "fn" true "varfn" true "varfn-" true
   "import" true "import*" true "use" true "require" true "dofile" true
   "quote" true "quasiquote" true})

(defn- requested? [position range]
  (and position (server-utils/position-in-range? position range)
       (not (server-utils/same-position?
              position (or (get range "end") (get range :end))))))

(defn- hint [position label tooltip &opt kind padding-left padding-right]
  {:position position :label label :kind (or kind 1)
   :paddingLeft (not (not padding-left))
   :paddingRight (not (not padding-right))
   :tooltip {:kind "markdown" :value tooltip}})

(defn- runtime-parameters [signature]
  (map |{:label $ :name $ :kind :positional}
       (array/slice
         (filter |(not (empty? $))
                 (string/split " " (string/trim signature "()")))
         1)))

(defn- call-signature [document context call-reference]
  (def static
    (signatures/find-in (get-in document [:analysis :signatures] @[])
                        (context :callee)))
  (def target
    (and call-reference
         (first (filter |(= (call-reference :identity) ($ :identity))
                        (get-in document [:analysis :index :definitions] @[])))))
  (or (and static target (= (target :name) (static :name)) static)
      (when (or (nil? call-reference) (nil? (call-reference :identity)))
        (when-let [signature
                   (doc/get-signature (symbol (context :callee))
                                      (document :eval-env))]
          (def parameters (runtime-parameters signature))
          {:parameters parameters :runtime true
           :variadic
           (any? (map |(string/has-prefix? "&" ($ :label)) parameters))}))))

(defn- parameter-hints [state document requested]
  (def content (document :content))
  (def result @[])
  (each reference (get-in document [:analysis :references] @[])
    (def byte-start (get-in reference [:range :start]))
    (def byte-end (get-in reference [:range :end]))
    (def position (position/byte->lsp-position content byte-start
                                               (state :position-encoding)))
    (when (requested? position requested)
      (when-let [context (lookup/call-context byte-end content)]
        (unless (or (get non-call-heads (context :callee))
                    (deep= byte-start
                           (lookup/from-index (first (context :range)) content)))
          (def call-start (lookup/from-index (first (context :range)) content))
          (def call-reference
            (first (filter |(deep= call-start (get-in $ [:range :start]))
                           (get-in document [:analysis :references] @[]))))
          (when-let [signature (call-signature document context call-reference)]
            (def parameters (signature :parameters))
            (def active
              (if (signature :runtime)
                (context :active-parameter)
                (signatures/active-parameter signature context content)))
            (when (and (not (signature :variadic))
                       (number? active) (< active (length parameters)))
              (def parameter (parameters active))
              (def label (parameter :label))
              # Janet named arguments already carry their label in source.
              (when (and (not= :named (parameter :kind))
                         (not= label (reference :name))
                         (not (string/has-prefix? "&" label)))
                (array/push
                  result
                  (hint position (string label ":")
                        (string "Parameter `" label "` of `"
                                (context :callee) "`")
                        2 false true)))))))))
  result)

(defn- literal-value [source definition]
  (when (has-value? ["def" "def-" "defglobal"] (definition :form))
    (try
      (let [start (lookup/to-index (get-in definition [:range :start]) source)
            end (lookup/to-index (get-in definition [:range :end]) source)
            parsed (parse (string/slice source start end))
            metadata (and (tuple? parsed)
                          (tuple/slice parsed 2 (dec (length parsed))))
            value (and (tuple? parsed) (last parsed))
            rendered (and (tuple? parsed) (>= (length parsed) 3)
                          (all |(or (keyword? $) (dictionary? $)) metadata)
                          (or (nil? value) (boolean? value) (number? value)
                              (keyword? value) (string? value))
                          (string/format "%q" value))]
        (and rendered (<= (length rendered) 32) rendered))
      ([_] nil))))

(defn- constant-hints [state document workspace requested]
  (def content (document :content))
  (def record (get-in document [:analysis :index]))
  (def result @[])
  (each reference (record :references)
    (when-let [identity (reference :identity)
               definition (index/definition-by-identity workspace identity)
               source (server-utils/content state (definition :uri))
               rendered (literal-value source definition)]
      (unless (and (= (definition :uri) (document :uri))
                   (deep= (definition :selection-range) (reference :range)))
        (def position
          (position/byte->lsp-position content (get-in reference [:range :end])
                                       (state :position-encoding)))
        (when (requested? position requested)
          (array/push result
                      (hint position (string " = " rendered)
                            (string "Constant value of `" (definition :name) "`")))))))
  result)

(defn- return-hints [state document requested]
  (def content (document :content))
  (catseq [definition :in (get-in document [:analysis :index :definitions] @[])
           :let [returns (definition :return-target)
                 position
                 (and returns
                      (position/byte->lsp-position
                        content (get-in definition [:selection-range :end])
                        (state :position-encoding)))]
           :when (and (= 12 (definition :kind)) returns
                      (requested? position requested))]
    (hint position (string " -> " returns)
          (string "Declared return metadata for `" (definition :name) "`")
          1 true false)))

(defn on-inlay-hint [state params]
  (def document (server-utils/document state params))
  (def workspace (server-utils/document-workspace state document))
  (def requested (get params "range"))
  (def result @[])
  (when (get-in state [:inlay-hints :parameterNames])
    (array/concat result (parameter-hints state document requested)))
  (when (get-in state [:inlay-hints :constantValues])
    (array/concat result (constant-hints state document workspace requested)))
  (when (get-in state [:inlay-hints :returnMetadata])
    (array/concat result (return-hints state document requested)))
  [:ok state
   (sort-by |[(get-in $ [:position :line]) (get-in $ [:position :character])
              (string ($ :label))]
            (distinct result))])
