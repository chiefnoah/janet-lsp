(import ./doc)
(import ./index)
(import ./logging)
(import ./lookup)
(import ./parser)
(import ./position)
(import ./server-utils)
(import ./utils)

(defmacro binding-to-lsp-item
  "Convert a Janet binding to an LSP CompletionItem."
  [name eval-env]
  (with-syms [$name $eval-env]
    ~(let [,$name ,name
           ,$eval-env ,eval-env
           value (get-in ,$eval-env [,$name :value] ,$name)]
       {:label ,$name
        :kind (case (type value)
                :symbol 12 :boolean 6
                :function 3 :cfunction 3
                :string 6 :buffer 6
                :number 6 :keyword 6
                :core/file 17 :core/peg 6
                :struct 6 :table 6
                :tuple 6 :array 6
                :fiber 6 :nil 6
                6)})))

(defn on-completion [state params]
  (let [document (server-utils/document state params)
        content (document :content)
        location (server-utils/request-byte-position state params content)
        word (lookup/word-at location content)
        prefix (string/slice (word :word) 0
                             (max 0 (- (location :character)
                                       (first (word :range)))))
        locals (parser/get-syms-at-loc location content)
        globals (seq [binding :in (all-bindings (document :eval-env))]
                  (binding-to-lsp-item binding (document :eval-env)))
        bindings (utils/concat-dedup-by-label locals globals)
        matching (if (empty? prefix)
                   bindings
                   (filter |(string/has-prefix? prefix (string ($ :label))) bindings))
        limit 2000
        items (array/slice matching 0 (min limit (length matching)))
        result {:isIncomplete (> (length matching) limit)
                :items (map |(merge $ {:data {:uri (document :uri)
                                              :version (document :version)
                                              :binding (string ($ :label))}})
                            items)}]
    (logging/message result [:completion] 1)
    [:ok state result]))

(defn on-completion-resolve [state params]
  (def label (or (get-in params ["data" "binding"]) (get params "label")))
  (def eval-env (get-in state [:documents (get-in params ["data" "uri"]) :eval-env]))
  (def result
    (merge params
           {"documentation"
            {:kind "markdown"
             :value (doc/my-doc* (symbol label) (or eval-env (make-env root-env)))}}))
  (logging/message result [:completion])
  [:ok state result])

(defn on-hover [state params]
  (let [document (server-utils/document state params)
        content (document :content)
        location (server-utils/request-byte-position state params content)
        word (lookup/word-at location content)
        hover-text (doc/my-doc* (symbol (word :word)) (document :eval-env))
        byte-range {:start {:line (location :line)
                            :character (first (word :range))}
                    :end {:line (location :line)
                          :character (last (word :range))}}
        result (if hover-text
                 {:contents {:kind "markdown" :value hover-text}
                  :range (server-utils/lsp-range state content byte-range)}
                 :null)]
    (logging/message result [:hover])
    [:ok state result]))

(defn- signature-parameters [signature]
  (map |{:label $}
       (array/slice
         (filter |(not (empty? $))
                 (string/split " " (string/trim signature "()")))
         1)))

(defn on-signature-help [state params]
  (let [document (server-utils/document state params)
        context (lookup/call-context
                  (server-utils/request-byte-position state params (document :content))
                  (document :content))
        callee (and context (context :callee))
        signature (and callee (doc/get-signature (symbol callee) (document :eval-env)))]
    (if (nil? signature)
      [:ok state :null]
      (let [parameters (signature-parameters signature)
            active (if (empty? parameters)
                     0
                     (min (context :active-parameter) (dec (length parameters))))]
        [:ok state
         {:signatures [{:label signature
                        :documentation {:kind "markdown"
                                        :value (doc/my-doc* (symbol callee)
                                                            (document :eval-env))}
                        :parameters parameters}]
          :activeSignature 0
          :activeParameter active}]))))

(defn- semantic-token-records [document]
  (def record (index/analyze (document :uri) (document :content)))
  (def definitions (record :definitions))
  (def records @[])
  (each definition definitions
    (array/push records {:range (definition :selection-range)
                         :type (if (= 12 (definition :kind)) 2 4)
                         :modifiers 3})
    (each parameter (definition :children)
      (array/push records {:range (parameter :selection-range)
                           :type 5 :modifiers 1})))
  (each reference (record :references)
    (def range (reference :range))
    (unless (any? (map |(deep= range ($ :range)) records))
      (def name (reference :name))
      (def definition (first (filter |(= name ($ :name)) definitions)))
      (def binding (get (document :eval-env) (symbol name) nil))
      (array/push records
                  {:range range
                   :type (cond
                           (string/has-prefix? ":" name) 6
                           (scan-number name) 8
                           (string/find "/" name) 0
                           definition (if (= 12 (definition :kind)) 2 4)
                           (and binding (binding :macro)) 3
                           (and binding
                                (has-value? [:function :cfunction]
                                            (type (binding :value)))) 2
                           4)
                   :modifiers 0})))
  (sort-by |[(get-in $ [:range :start :line])
             (get-in $ [:range :start :character])]
           records))

(defn on-semantic-tokens-full [state params]
  (def document (server-utils/document state params))
  (def data @[])
  (var previous-line 0)
  (var previous-character 0)
  (each token (semantic-token-records document)
    (def range (server-utils/lsp-range state (document :content) (token :range)))
    (def start (range :start))
    (def end (range :end))
    (when (and start end (= (start :line) (end :line))
               (> (end :character) (start :character)))
      (def delta-line (- (start :line) previous-line))
      (def delta-character (if (= delta-line 0)
                             (- (start :character) previous-character)
                             (start :character)))
      (array/concat data [delta-line delta-character
                          (- (end :character) (start :character))
                          (token :type) (token :modifiers)])
      (set previous-line (start :line))
      (set previous-character (start :character))))
  [:ok state {:data data}])

(defn- missing-delimiters [source]
  (def stack @[])
  (each byte (string/bytes (lookup/structure-mask source))
    (case byte
      40 (array/push stack 41)
      91 (array/push stack 93)
      123 (array/push stack 125)
      41 (if (and (not (empty? stack)) (= 41 (last stack)))
           (array/pop stack) (array/push stack nil))
      93 (if (and (not (empty? stack)) (= 93 (last stack)))
           (array/pop stack) (array/push stack nil))
      125 (if (and (not (empty? stack)) (= 125 (last stack)))
            (array/pop stack) (array/push stack nil))))
  (when (not (has-value? stack nil))
    (string/from-bytes ;(reverse stack))))

(defn on-code-action [state params]
  (def document (server-utils/document state params))
  (def only (get-in params ["context" "only"] @[]))
  (def actions @[])
  (when (or (empty? only) (has-value? only "quickfix"))
    (each diagnostic (get-in params ["context" "diagnostics"] @[])
      (when (and (= "janet.parse.unclosed-delimiter" (get diagnostic "code"))
                 (= (hash (document :content))
                    (get-in diagnostic ["data" "contentHash"]))
                 (= (document :version) (get-in diagnostic ["data" "version"])))
        (when-let [closing (missing-delimiters (document :content))]
          (when (not (empty? closing))
            (def end (position/document-end (document :content)
                                            (state :position-encoding)))
            (array/push actions
                        {:title (string "Insert missing " closing)
                         :kind "quickfix"
                         :diagnostics [diagnostic]
                         :isPreferred true
                         :edit {:documentChanges
                                [(server-utils/versioned-edit
                                   state (document :uri)
                                   [{:range {:start end :end end}
                                     :newText closing}])]}}))))))
  [:ok state actions])

(defn on-inlay-hint [state params]
  (if (not (state :inlay-parameter-hints))
    [:ok state @[]]
    (let [document (server-utils/document state params)
          content (document :content)
          requested (get params "range")
          hints @[]]
      (each reference ((index/analyze (document :uri) content) :references)
        (def byte-start (get-in reference [:range :start]))
        (def byte-end (get-in reference [:range :end]))
        (def position (position/byte->lsp-position content byte-start
                                                   (state :position-encoding)))
        (when (and position (server-utils/position-in-range? position requested))
          (def context (lookup/call-context byte-end content))
          (when (and context
                     (not (deep= byte-start
                                 (lookup/from-index (first (context :range)) content))))
            (when-let [signature
                       (doc/get-signature (symbol (context :callee))
                                          (document :eval-env))]
              (def parameters (map |($ :label) (signature-parameters signature)))
              (def active (context :active-parameter))
              (when (and (< active (length parameters))
                         (not (any? (map |(string/has-prefix? "&" $) parameters))))
                (def parameter (parameters active))
                (when (not= parameter (reference :name))
                  (array/push hints
                              {:position position
                               :label (string parameter ":")
                               :kind 2
                               :paddingRight true
                               :tooltip {:kind "markdown"
                                         :value (string "Parameter `" parameter "` of `"
                                                        (context :callee) "`")}})))))))
      [:ok state (distinct hints)])))
