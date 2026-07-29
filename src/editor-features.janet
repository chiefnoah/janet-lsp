(import ./doc)
(import ./analysis)
(import ./completion)
(import ./logging)
(import ./lookup)
(import ./position)
(import ./server-utils)
(import ./semantic-tokens)
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

(defn- semantic-data [state params request-id &opt requested]
  (def document (server-utils/document state params))
  (def workspace (server-utils/document-workspace state document))
  (def snapshot (or (analysis/current document workspace)
                    (analysis/refresh document workspace
                                      (state :position-encoding))))
  (if (and request-id (has-key? (state :cancelled-requests) request-id))
    [:cancelled]
    [:ok (semantic-tokens/encode state (document :content)
                                 (snapshot :semantic) requested request-id)]))

(defn- remember-semantic-result [state result-id data]
  (unless (get (state :semantic-token-results) result-id)
    (put (state :semantic-token-results) result-id data)
    (array/push (state :semantic-token-order) result-id))
  (while (> (length (state :semantic-token-order)) 16)
    (def oldest ((state :semantic-token-order) 0))
    (array/remove (state :semantic-token-order) 0)
    (put (state :semantic-token-results) oldest nil)))

(defn on-semantic-tokens-full [state params request-id]
  (match (semantic-data state params request-id)
    [:cancelled] [:rpc-error state -32800 "Request cancelled"]
    [:ok data]
    (let [document (server-utils/document state params)
          result-id (semantic-tokens/result-id (document :analysis)
                                               (state :position-encoding) data)]
      (remember-semantic-result state result-id data)
      [:ok state {:data data :resultId result-id}])))

(defn on-semantic-tokens-delta [state params request-id]
  (match (semantic-data state params request-id)
    [:cancelled] [:rpc-error state -32800 "Request cancelled"]
    [:ok data]
    (let [document (server-utils/document state params)
          result-id (semantic-tokens/result-id (document :analysis)
                                               (state :position-encoding) data)
          previous (get (state :semantic-token-results)
                        (get params "previousResultId"))]
      (remember-semantic-result state result-id data)
      (if previous
        [:ok state {:resultId result-id
                    :edits (semantic-tokens/delta previous data)}]
        [:ok state {:resultId result-id :data data}]))))

(defn on-semantic-tokens-range [state params request-id]
  (match (semantic-data state params request-id (get params "range"))
    [:cancelled] [:rpc-error state -32800 "Request cancelled"]
    [:ok data] [:ok state {:data data}]))

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
