(import ../libs/fmt)
(import ./analysis)
(import ./index)
(import ./logging)
(import ./lookup)
(import ./position)
(import ./server-utils)
(import ./uri)

(defn- publish [state document diagnostics &opt version]
  (def message
    {:method "textDocument/publishDiagnostics"
     :params (if version
               {:uri (document :uri) :version version :diagnostics diagnostics}
               {:uri (document :uri) :diagnostics diagnostics})})
  (logging/message message [:diagnostics])
  [:ok state message :notify true])

(defn- integer? [value]
  (and (number? value) (= value (math/floor value))))

(defn apply-changes [content changes encoding]
  (if (or (not (indexed? changes)) (empty? changes))
    nil
    (do
      (var updated content)
      (var valid true)
      (each change changes
        (when valid
          (def text (get change "text"))
          (def range (get change "range"))
          (cond
            (not (string? text))
            (set valid false)

            (nil? range)
            (set updated text)

            (let [start (position/lsp->byte-position
                          updated (get range "start") encoding)
                  end (position/lsp->byte-position
                        updated (get range "end") encoding)]
              (if (or (nil? start) (nil? end))
                (set valid false)
                (let [start-index (lookup/to-index start updated)
                      end-index (lookup/to-index end updated)
                      range-length (get change "rangeLength")]
                  (if (or (> start-index end-index)
                          (and (not (nil? range-length))
                               (or (not (integer? range-length))
                                   (not= range-length
                                         (position/text-units
                                           (string/slice updated start-index end-index)
                                           encoding)))))
                    (set valid false)
                    (set updated
                         (string (string/slice updated 0 start-index)
                                 text
                                 (string/slice updated end-index))))))))))
      (if valid updated nil))))

(defn- refresh [state document workspace]
  (analysis/refresh document workspace (state :position-encoding)))

(defn- diagnostic-message [document snapshot &opt diagnostics]
  {:method "textDocument/publishDiagnostics"
   :params {:uri (document :uri)
            :version (document :version)
            :diagnostics (or diagnostics (snapshot :diagnostics))}})

(defn- push-with-dependents [state document workspace snapshot &opt diagnostics]
  (def notifications @[(diagnostic-message document snapshot diagnostics)])
  (each dependent (values (state :documents))
    (when (and (not= dependent document)
               (= workspace (server-utils/document-workspace state dependent)))
      (def previous (get-in dependent [:analysis :diagnostics]))
      (def current
        (or (analysis/current dependent workspace)
            (refresh state dependent workspace)))
      (unless (deep= previous (current :diagnostics))
        (array/push notifications (diagnostic-message dependent current)))))
  [:notifications state notifications])

(defn- resynchronizing-changes [changes]
  (when (indexed? changes)
    (var replacement nil)
    (eachp [index change] changes
      (when (nil? (get change "range")) (set replacement index)))
    (and replacement (array/slice changes replacement))))

(defn on-change [state params]
  (let [document (server-utils/document state params)
        version (get-in params ["textDocument" "version"])
        current-version (and document (document :version))
        changes (get params "contentChanges")
        effective-changes (if (and document (document :desynchronized))
                            (resynchronizing-changes changes)
                            changes)]
    (if (or (nil? document)
            (not (integer? version))
            (and (integer? current-version) (<= version current-version))
            (and (document :desynchronized) (nil? effective-changes)))
      [:noresponse state]
      (let [workspace (server-utils/document-workspace state document)
            content (apply-changes (document :content)
                                   effective-changes
                                   (state :position-encoding))]
        (if (nil? content)
          (do
            (put document :desynchronized true)
            (logging/warn "Ignoring invalid incremental document change" [:change])
            [:noresponse state])
          (do
            (merge-into document {:content content :version version
                                  :desynchronized false})
            (def snapshot (refresh state document workspace))
            (if (dyn :push-diagnostics)
              (push-with-dependents state document workspace snapshot)
              [:noresponse state])))))))

(defn on-close [state params]
  (if-let [document (server-utils/document state params)]
    (do
      (def workspace (server-utils/document-workspace state document))
      (put (state :documents) (server-utils/document-uri params) nil)
      (analysis/replace-record workspace (document :uri)
                               (get-in workspace [:disk-index (document :uri)]))
      (if (dyn :push-diagnostics)
        (push-with-dependents state document workspace (document :analysis) @[])
        [:noresponse state]))
    [:noresponse state]))

(defn on-diagnostic [state params]
  (if-let [document (server-utils/document state params)]
    (let [workspace (server-utils/document-workspace state document)
          snapshot (or (analysis/current document workspace)
                       (refresh state document workspace))
          result-id (snapshot :diagnostic-result-id)
          report (if (= result-id (get params "previousResultId"))
                   {:kind "unchanged" :resultId result-id}
                   {:kind "full" :resultId result-id
                    :items (snapshot :diagnostics)})]
      (logging/message report [:diagnostics])
      [:ok state report])
    [:ok state :null]))

(defn- workspace-document-uris [state]
  (def found @{})
  (each workspace (values (state :workspaces))
    (each document-uri (keys (workspace :index))
      (put found document-uri true)))
  (each document-uri (keys (state :documents))
    (put found document-uri true))
  (sort (keys found)))

(defn- workspace-snapshot [state document-uri]
  (if-let [document (get (state :documents) document-uri)]
    (let [workspace (server-utils/document-workspace state document)]
      [(or (analysis/current document workspace)
           (refresh state document workspace))
       (document :version)])
    (when-let [content (server-utils/content state document-uri)
               filepath (uri/file-uri->path document-uri)]
      (let [workspace (server-utils/workspace-for-path state filepath)
            restricted (merge workspace {:trusted false})
            document {:uri document-uri :path filepath :content content :version nil}]
        [(analysis/build document restricted (state :position-encoding)) :null]))))

(defn on-workspace-diagnostic [state params]
  (def previous @{})
  (each item (get params "previousResultIds" @[])
    (when (and (string? (get item "uri")) (string? (get item "value")))
      (put previous (get item "uri") (get item "value"))))
  (def reports @[])
  (def reported @{})
  (each document-uri (workspace-document-uris state)
    (when-let [[snapshot version] (workspace-snapshot state document-uri)]
      (put reported document-uri true)
      (def result-id (snapshot :diagnostic-result-id))
      (array/push
        reports
        (if (= result-id (get previous document-uri))
          {:uri document-uri :version version :kind "unchanged" :resultId result-id}
          {:uri document-uri :version version :kind "full" :resultId result-id
           :items (snapshot :diagnostics)}))))
  (each document-uri (sort (keys previous))
    (unless (get reported document-uri)
      (def result-id
        (string (state :diagnostic-generation) ":removed:"
                (index/content-hash document-uri)))
      (array/push
        reports
        (if (= result-id (get previous document-uri))
          {:uri document-uri :version :null :kind "unchanged" :resultId result-id}
          {:uri document-uri :version :null :kind "full" :resultId result-id
           :items @[]}))))
  [:ok state {:items reports}])

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
        document @{:content content
                   :version version
                   :uri document-uri
                   :path filepath
                   :desynchronized false
                   :snapshots @{}
                   :snapshot-order @[]}
        snapshot (refresh state document workspace)]
    (put (state :documents) document-uri document)
    (logging/info "Document opened" [:open] 1)
    (if (dyn :push-diagnostics)
      (push-with-dependents state document workspace snapshot)
      [:noresponse state])))
