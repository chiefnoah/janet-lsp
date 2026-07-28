(import ../libs/fmt)
(import ./diagnostics)
(import ./index)
(import ./logging)
(import ./position)
(import ./server-utils)
(import ./uri)

(defn- publish [state document diagnostics &opt version]
  (def message {:method "textDocument/publishDiagnostics"
                :params (if version
                          {:uri (document :uri)
                           :version version
                           :diagnostics diagnostics}
                          {:uri (document :uri)
                           :diagnostics diagnostics})})
  (logging/message message [:diagnostics])
  [:ok state message :notify true])

(defn on-change [state params]
  (let [document (server-utils/document state params)
        version (get-in params ["textDocument" "version"])
        current-version (and document (document :version))]
    (if (or (nil? document)
            (and (number? version) (number? current-version)
                 (<= version current-version)))
      [:noresponse state]
      (let [workspace (server-utils/document-workspace state document)
            content (get-in params ["contentChanges" 0 "text"])
            [diagnostics env]
            (diagnostics/run (document :path) content (state :position-encoding)
                             workspace version)]
        (merge-into document {:content content :version version :eval-env env})
        (index/update workspace (document :uri) content)
        (if (dyn :push-diagnostics)
          (publish state document diagnostics version)
          [:noresponse state])))))

(defn on-close [state params]
  (if-let [document (server-utils/document state params)]
    (do
      (put (state :documents) (server-utils/document-uri params) nil)
      (if (dyn :push-diagnostics)
        (publish state document @[])
        [:noresponse state]))
    [:noresponse state]))

(defn on-diagnostic [state params]
  (if-let [document (server-utils/document state params)]
    (let [workspace (server-utils/document-workspace state document)
          [items env]
          (diagnostics/run (document :path) (document :content)
                           (state :position-encoding) workspace (document :version))
          report {:kind "full" :items items}]
      (put document :eval-env env)
      (logging/message report [:diagnostics])
      [:ok state report])
    [:ok state :null]))

(defn on-formatting [state params]
  (def document (server-utils/document state params))
  (def content (document :content))
  # Janet formatting is canonical; LSP indentation options are advisory.
  (match (try [:ok (freeze (fmt/format (string/slice content)))]
           ([err] [:error err]))
    [:error err]
    (do
      (logging/warn (string/format "formatter failed: %s" err) [:formatting])
      [:ok state :null])
    [:ok formatted]
    (if (= content formatted)
      [:ok state @[]]
      [:ok state [{:range {:start {:line 0 :character 0}
                           :end (position/document-end content
                                                       (state :position-encoding))}
                   :newText formatted}]])))

(defn on-open [state params]
  (let [content (get-in params ["textDocument" "text"])
        document-uri (server-utils/document-uri params)
        version (get-in params ["textDocument" "version"])
        filepath (uri/file-uri->path document-uri)
        workspace (server-utils/workspace-for-path state filepath)
        [items env]
        (diagnostics/run filepath content (state :position-encoding) workspace version)
        document @{:content content
                   :version version
                   :uri document-uri
                   :path filepath
                   :eval-env env}]
    (put (state :documents) document-uri document)
    (index/update workspace document-uri content)
    (logging/info "Document opened" [:open] 1)
    (if (dyn :push-diagnostics)
      (publish state document items version)
      [:noresponse state])))
