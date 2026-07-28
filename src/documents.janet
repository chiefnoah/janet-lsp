(import ../libs/fmt)
(import ./analysis)
(import ./index)
(import ./logging)
(import ./lookup)
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
              (publish state document (snapshot :diagnostics) version)
              [:noresponse state])))))))

(defn on-close [state params]
  (if-let [document (server-utils/document state params)]
    (do
      (def workspace (server-utils/document-workspace state document))
      (put (state :documents) (server-utils/document-uri params) nil)
      (if-let [record (get-in workspace [:disk-index (document :uri)])]
        (index/update-record workspace (document :uri) record)
        (index/remove workspace (document :uri)))
      (if (dyn :push-diagnostics)
        (publish state document @[])
        [:noresponse state]))
    [:noresponse state]))

(defn on-diagnostic [state params]
  (if-let [document (server-utils/document state params)]
    (let [workspace (server-utils/document-workspace state document)
          snapshot (or (analysis/current document workspace)
                       (refresh state document workspace))
          report {:kind "full" :items (snapshot :diagnostics)}]
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
      (publish state document (snapshot :diagnostics) version)
      [:noresponse state])))
