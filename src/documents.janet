(import ../libs/fmt)
(import ./analysis)
(import ./document-features)
(import ./index)
(import ./logging)
(import ./lookup)
(import ./position)
(import ./request-control)
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

(defn on-diagnostic [state params &opt request-id]
  (try
    (do
      (request-control/checkpoint state request-id)
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
      (request-control/checkpoint state request-id)
      [:ok state report])
    [:ok state :null]))
    ([err] (if (= :request-cancelled err)
             [:rpc-error state -32800 "Request cancelled" nil]
             (error err)))))

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

(defn- workspace-diagnostic [state params request-id]
  (def previous @{})
  (each item (get params "previousResultIds" @[])
    (when (and (string? (get item "uri")) (string? (get item "value")))
      (put previous (get item "uri") (get item "value"))))
  (def reports @[])
  (def reported @{})
  (each document-uri (workspace-document-uris state)
    (request-control/checkpoint state request-id)
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

(defn on-workspace-diagnostic [state params &opt request-id]
  (try (workspace-diagnostic state params request-id)
    ([err] (if (= :request-cancelled err)
             [:rpc-error state -32800 "Request cancelled" nil]
             (error err)))))

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

(defn- format-source [source]
  (try (freeze (fmt/format (string/slice source)))
    ([err]
      (logging/warn (string/format "formatter failed: %s" err) [:formatting])
      nil)))

(defn- format-spans [content start end]
  (def scanned (document-features/scan content))
  (def forms (filter |($ :complete) (scanned :forms)))
  (def atomic-spans
    (array
      ;(map |{:start (or ($ :literal-start) ($ :start))
              :end (or ($ :literal-end) ($ :end))}
            (scanned :tokens))
      ;(scanned :comments)))
  (def contained
    (sort-by |[($ :start) ($ :end)]
             (filter |(and (<= start ($ :start)) (<= ($ :end) end)) forms)))
  (def outer-contained
    (filter
      (fn [candidate]
        (not (any? (map |(and (not= candidate $)
                              (<= ($ :start) (candidate :start))
                              (<= (candidate :end) ($ :end)))
                        contained))))
      contained))
  (def enclosing
    (first
      (sort-by |(- ($ :end) ($ :start))
               (filter |(and (<= ($ :start) start) (<= end ($ :end))) forms))))
  (cond
    (not (empty? outer-contained))
    (array
      ;[{:start (get-in outer-contained [0 :start])
         :end ((last outer-contained) :end) :complete true}]
      ;(if enclosing [enclosing] @[]))

    enclosing [enclosing]

    (and (< start end)
         (not
           (any?
              (map |(or (and (< ($ :start) start) (< start ($ :end)))
                        (and (< ($ :start) end) (< end ($ :end))))
                   atomic-spans)))
         (not
           (any? (map |(and (not ($ :complete))
                            (< ($ :start) end) (< start ($ :end)))
                      (scanned :forms)))))
    [{:start start :end end :complete true}]

    @[]))

(defn- utf-8-boundary? [bytes index]
  (or (= index 0) (= index (length bytes))
      (not= 128 (band (bytes index) 192))))

(defn- format-span-edit [state content span &opt requested-start requested-end]
  (def original (string/slice content (span :start) (span :end)))
  (def start-position (lookup/from-index (span :start) content))
  # Isolated multiline forms cannot reproduce an enclosing nonzero column safely.
  (unless (and (> (start-position :character) 0) (string/find "\n" original))
    (when-let [raw-formatted (format-source original)]
      (def formatted
        (if (and (not (string/has-suffix? "\n" original))
                 (string/has-suffix? "\n" raw-formatted))
          (string/slice raw-formatted 0 (dec (length raw-formatted)))
          raw-formatted))
      (unless (= original formatted)
        (def original-bytes (string/bytes original))
        (def formatted-bytes (string/bytes formatted))
        (var prefix 0)
        (while (and (< prefix (length original-bytes))
                    (< prefix (length formatted-bytes))
                    (= (original-bytes prefix) (formatted-bytes prefix)))
          (+= prefix 1))
        (while (and (> prefix 0)
                    (or (not (utf-8-boundary? original-bytes prefix))
                        (not (utf-8-boundary? formatted-bytes prefix))))
          (-= prefix 1))
        (var suffix 0)
        (while (and (< (+ prefix suffix) (length original-bytes))
                    (< (+ prefix suffix) (length formatted-bytes))
                    (= (original-bytes (- (dec (length original-bytes)) suffix))
                       (formatted-bytes (- (dec (length formatted-bytes)) suffix))))
          (+= suffix 1))
        (while (and (> suffix 0)
                    (or (not (utf-8-boundary?
                               original-bytes (- (length original-bytes) suffix)))
                        (not (utf-8-boundary?
                               formatted-bytes (- (length formatted-bytes) suffix)))))
          (-= suffix 1))
        (def edit-start (+ (span :start) prefix))
        (def edit-end (- (span :end) suffix))
        (when (and (or (nil? requested-start) (<= requested-start edit-start))
                   (or (nil? requested-end) (<= edit-end requested-end)))
          {:range (server-utils/lsp-range
                    state content
                    {:start (lookup/from-index edit-start content)
                     :end (lookup/from-index edit-end content)})
           :newText (string/slice formatted prefix
                                  (- (length formatted) suffix))})))))

(defn on-range-formatting [state params]
  (def document (server-utils/document state params))
  (def content (document :content))
  (def requested (get params "range"))
  (def start-position
    (position/lsp->byte-position content (get requested "start")
                                 (state :position-encoding)))
  (def end-position
    (position/lsp->byte-position content (get requested "end")
                                 (state :position-encoding)))
  (if (and start-position end-position)
    (let [start (lookup/to-index start-position content)
          end (lookup/to-index end-position content)]
      (if (<= start end)
        (if-let [edit
                 (some |(format-span-edit state content $ start end)
                       (format-spans content start end))]
          [:ok state [edit]]
          [:ok state @[]])
        [:ok state :null]))
    [:ok state :null]))

(defn on-type-formatting [state params]
  (def document (server-utils/document state params))
  (def content (document :content))
  (def trigger (get params "ch"))
  (def byte-position (server-utils/request-byte-position state params content))
  (if (and byte-position (has-value? [")" "]" "}"] trigger))
    (let [cursor (lookup/to-index byte-position content)
          bytes (string/bytes content)]
      (if (and (> cursor 0) (= (first (string/bytes trigger)) (bytes (dec cursor))))
        (let [forms ((document-features/scan content) :forms)
              incomplete (filter |(not ($ :complete)) forms)
              candidates
              (filter
                (fn [candidate]
                  (and (candidate :complete) (= cursor (candidate :end))
                       (= (get document-features/close-to-open
                               (first (string/bytes trigger)))
                          (candidate :open))
                       (not
                         (any?
                           (map (fn [ancestor]
                                  (and (<= (ancestor :start) (candidate :start))
                                       (<= (candidate :end) (ancestor :end))))
                                incomplete)))))
                forms)
              typed (first (sort-by |($ :start) candidates))
              span
              (and typed
                   (first
                     (sort-by |($ :start)
                              (filter |(and ($ :complete)
                                            (<= ($ :start) (typed :start))
                                            (<= (typed :end) ($ :end)))
                                      forms))))]
          (if-let [edit (and span (format-span-edit state content span))]
            [:ok state [edit]]
            [:ok state @[]]))
        [:ok state @[]]))
    [:ok state @[]]))

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
