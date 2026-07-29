(import ./doc)
(import ./analysis)
(import ./completion)
(import ./index)
(import ./logging)
(import ./lookup)
(import ./position)
(import ./server-utils)
(import ./signatures)

(defn binding-to-lsp-item
  "Convert a Janet binding to an LSP CompletionItem."
  [name eval-env]
  (completion/binding-item name eval-env))

(defn on-completion [state params]
  (let [document (server-utils/document state params)
        content (document :content)
        location (server-utils/request-byte-position state params content)
        result (completion/complete state document location)]
    (logging/message result [:completion] 1)
    [:ok state result]))

(defn on-completion-resolve [state params]
  (def label (or (get-in params ["data" "binding"]) (get params "label")))
  (def document (get-in state [:documents (get-in params ["data" "uri"])]))
  (def snapshot-key (get-in params ["data" "snapshot"]))
  (def eval-env
    (if snapshot-key
      (get-in (analysis/find-snapshot document snapshot-key) [:eval-env])
      (and document (document :eval-env))))
  (def documentation
    (and (or (nil? snapshot-key) eval-env)
         (doc/my-doc* (symbol label) (or eval-env (make-env root-env)))))
  (var result (if documentation
                (merge params {"documentation" {:kind "markdown"
                                                 :value documentation}})
                params))
  (def additional-key
    (first (filter |(= "additionalTextEdits" (string $)) (keys result))))
  (when (and additional-key
             (or (nil? document)
                 (not= (document :version) (get-in params ["data" "version"]))))
    (set result (merge result))
    (put result additional-key nil))
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
        static-signature
        (and callee
             (signatures/find-in (get-in document [:analysis :signatures] @[])
                                 callee))
        runtime-signature
        (and (not static-signature) callee
             (doc/get-signature (symbol callee) (document :eval-env)))]
    (if (and (nil? static-signature) (nil? runtime-signature))
      [:ok state :null]
      (let [label (if static-signature
                    (static-signature :label)
                    runtime-signature)
            parameters (if static-signature
                         (map |{:label ($ :label)}
                              (static-signature :parameters))
                         (signature-parameters runtime-signature))
            active (if (empty? parameters)
                     0
                     (if static-signature
                       (signatures/active-parameter
                         static-signature context (document :content))
                       (min (context :active-parameter) (dec (length parameters)))))
            documentation (doc/my-doc* (symbol callee) (document :eval-env))
            signature-info
            (merge {:label label :parameters parameters}
                   (if documentation
                     {:documentation {:kind "markdown" :value documentation}}
                     {}))]
        [:ok state
         {:signatures [signature-info]
          :activeSignature 0
          :activeParameter active}]))))

(defn on-semantic-tokens-full [state params]
  (def document (server-utils/document state params))
  (def data @[])
  (var previous-line 0)
  (var previous-character 0)
  (each token (get-in document [:analysis :semantic] @[])
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
                 (= (index/content-hash (document :content))
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
      (each reference (get-in document [:analysis :references] @[])
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
