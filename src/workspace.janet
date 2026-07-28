(import ./analysis)
(import ./configuration)
(import ./index)
(import ./index-cache)
(import ./logging)
(import ./rpc)
(import ./server-utils)
(import ./transport)
(import ./uri)
(import spork/path)

(def indexer-script
  (path/join (path/dirname (dyn :current-file)) "indexer.janet"))

(varfn reanalyze-open-documents [])

(defn- janet-executable []
  (def executable (dyn :executable))
  (or (and (os/stat executable) (path/abspath executable))
      (first (filter os/stat
                     (map |(path/join $ executable)
                          (string/split (if (= :windows (os/which)) ";" ":")
                                        (or (os/getenv "PATH") "")))))
      executable))

(defn find-all-module-files [filepath &opt search-jpm-tree explicit results]
  (default explicit true)
  (default results @[])
  (case (os/stat filepath :mode)
    :directory
    (when (or explicit search-jpm-tree (not= (path/basename filepath) "jpm_tree"))
      (each entry (os/dir filepath)
        (find-all-module-files (path/join filepath entry)
                               search-jpm-tree false results)))
    :file
    (when (and (or explicit (not= (path/basename filepath) "project.janet"))
               (any? (map |(string/has-suffix? $ filepath)
                           [".janet" ".jimage" ".so"])))
      (array/push results filepath)))
  results)

(defn find-unique-paths [paths]
  (->> (seq [found-path :in paths]
         (let [directory (path/dirname found-path)
               wildcard (path/join directory (string ":all:" (path/ext found-path)))]
           (if (= (path/basename found-path) "init.janet")
             [wildcard found-path]
             [wildcard])))
       flatten
       distinct))

(defn configure [root-uri trusted-workspaces &opt diagnostic-settings
                 diagnostic-generation]
  (default diagnostic-settings (configuration/diagnostics {}))
  (default diagnostic-generation 0)
  (def root-path (uri/file-uri->path root-uri))
  (def trusted (and root-path (has-value? trusted-workspaces root-uri)))
  (var workspace-env (make-env root-env))
  (def unique-paths
    (if trusted
      (find-unique-paths
        (find-all-module-files root-path (not ((dyn :opts) :dont-search-jpm-tree))))
      @[]))
  (when trusted
    (def startup-path (path/join root-path ".janet-lsp" "startup.janet"))
    (when (os/stat startup-path)
      (set workspace-env (dofile startup-path :env workspace-env))))
  @{:uri root-uri
    :path root-path
    :trusted (not (not trusted))
    :diagnostic-settings diagnostic-settings
    :diagnostic-generation diagnostic-generation
    :trust-prompted false
    :index @{}
    :disk-index @{}
    :cache-path (index-cache/path-for root-uri)
    :cache-current false
    :exclusions index/default-exclusions
    :env workspace-env
    :unique-paths unique-paths})

(defn- copy-index [records]
  (def copied @{})
  (eachp [document-uri record] records
    (put copied document-uri record))
  copied)

(defn load-cache [workspace]
  (try
    (let [loaded (index-cache/load (workspace :cache-path)
                                   (workspace :uri)
                                   (workspace :path)
                                   (workspace :exclusions))
          disk-index (loaded :index)]
      (put workspace :disk-index disk-index)
      (put workspace :index (copy-index disk-index))
      (put workspace :cache-current (loaded :complete)))
    ([err]
      (logging/warn (string "Could not load workspace index cache: " err) [:index])
      (put workspace :disk-index @{})
      (put workspace :index @{})
      (put workspace :cache-current false)))
  workspace)

(defn partition-indexes [state]
  (each workspace (values (state :workspaces))
    (put workspace :index (copy-index (workspace :disk-index)))
    (each document-uri (keys (workspace :index))
      (when-let [filepath (uri/file-uri->path document-uri)]
        (when (not= workspace (server-utils/workspace-for-path state filepath))
          (put (workspace :index) document-uri nil))))
    (index/relink workspace))
  state)

(defn merge-scan-changes [disk-index changes]
  (eachp [document-uri change] changes
    (if (= :deleted change)
      (put disk-index document-uri nil)
      (put disk-index document-uri change)))
  (index/relink @{:index disk-index})
  disk-index)

(defn- progress [token value]
  (transport/write-frame stdout
                         (rpc/notification
                           {:method "$/progress" :params {:token token :value value}})))

(defn start-scans [state &opt workspaces]
  (default workspaces (values (state :workspaces)))
  (def requests @[])
  (each workspace workspaces
    (unless (or (workspace :scan) (workspace :cache-current))
      (def digest (hash (workspace :uri)))
      (def token (string "janet-lsp/index/" digest))
      (def output (string "/tmp/janet-lsp-index-" (os/getpid) "-" digest ".jdn"))
      (each stale [output (string output ".tmp")]
        (when (os/stat stale) (os/rm stale)))
      (def process
        (try (os/spawn [(janet-executable) indexer-script
                        (workspace :path) output
                        (string/format "%j" (workspace :exclusions))
                        (workspace :cache-path) (workspace :uri)])
          ([err]
            (logging/warn (string/format "Could not start workspace indexer: %s" err)
                          [:index])
            nil)))
      (when process
        (merge-into workspace {:scan-token token
                               :scan-output output
                               :scan process
                               :scan-changes @{}
                               :progress-started false})
        (when (state :work-done-progress)
          (def id (string "janet-lsp/progress/create/" digest))
          (put (state :pending-requests) id
               {:kind :progress-create :uri (workspace :uri) :token token})
          (array/push requests
                      {:id id
                       :method "window/workDoneProgress/create"
                       :params {:token token}})))))
  requests)

(defn refresh-scans [state]
  (each workspace (values (state :workspaces))
    (when-let [output (workspace :scan-output)
               found (os/stat output)]
      (def result
        (try (let [parsed (parse (slurp output))]
               (if (dictionary? parsed)
                 parsed
                 {:ok false :error "invalid indexer output"}))
          ([err] {:ok false :error (string "invalid indexer output: " err)})))
      (os/rm output)
      (def status (os/proc-wait (workspace :scan)))
      (def succeeded (and (= true (result :ok)) (= 0 status)))
      (when succeeded
        (def disk-index (result :index))
        (merge-scan-changes disk-index (workspace :scan-changes))
        (put workspace :disk-index disk-index)
        (put workspace :index (copy-index (workspace :disk-index)))
        (def cached
          (try
            (index-cache/write (workspace :cache-path) (workspace :uri)
                               (workspace :exclusions) disk-index)
            ([_] false)))
        (put workspace :cache-current cached))
      (put workspace :scan nil)
      (put workspace :scan-output nil)
      (put workspace :scan-changes nil)
      (if succeeded
        (do
          (partition-indexes state)
          (reanalyze-open-documents state))
        (each document (values (state :documents))
          (when (= workspace (server-utils/document-workspace state document))
            (when-let [snapshot (analysis/current document workspace)]
              (index/update-record workspace (document :uri) (snapshot :index))))))
      (unless succeeded
        (logging/warn (string "Workspace index failed: "
                              (or (result :error) (string "exit status " status)))
                      [:index]))
      (when (workspace :progress-started)
        (progress (workspace :scan-token)
                  {:kind "end"
                   :message (if succeeded "Index complete" "Index failed")}))
      (put workspace :progress-started false)))
  state)

(defn- remove-scan-files [workspace]
  (each output [(workspace :scan-output)
                (and (workspace :scan-output) (string (workspace :scan-output) ".tmp"))]
    (when (and output (os/stat output)) (os/rm output))))

(defn- stop-scan [workspace]
  (when (workspace :scan)
    (try (os/proc-kill (workspace :scan) true) ([_] nil)))
  (remove-scan-files workspace)
  (put workspace :scan nil)
  (put workspace :scan-output nil)
  (put workspace :scan-changes nil))

(defn cancel-scan [state params]
  (def token (get params "token"))
  (each workspace (values (state :workspaces))
    (when (and (= token (workspace :scan-token)) (workspace :scan))
      (when (workspace :progress-started)
        (progress token {:kind "end" :message "Index cancelled"}))
      (stop-scan workspace)))
  [:noresponse state])

(defn stop-scans [state]
  (each workspace (values (state :workspaces))
    (stop-scan workspace))
  state)

(defn on-watched-files-changed [state params]
  (def changed-workspaces @[])
  (each change (get params "changes")
    (def document-uri (get change "uri"))
    (def filepath (uri/file-uri->path document-uri))
    (def affected
      (filter |(and filepath
                    (server-utils/path-in-workspace? filepath ($ :path)))
              (values (state :workspaces))))
    (each workspace affected
      (array/push changed-workspaces workspace)
      (case (get change "type")
        3 (do
            (index/remove @{:index (workspace :disk-index)} document-uri)
            (when (workspace :scan)
              (put (workspace :scan-changes) document-uri :deleted)))
        (if (and filepath (os/stat filepath) (string/has-suffix? ".janet" filepath))
          (try
            (let [record (index/analyze document-uri (slurp filepath))]
              (index/update-record @{:index (workspace :disk-index)}
                                   document-uri record)
              (when (workspace :scan)
                (put (workspace :scan-changes) document-uri record)))
            ([_]
              (index/remove @{:index (workspace :disk-index)} document-uri)
              (when (workspace :scan)
                (put (workspace :scan-changes) document-uri :deleted))))
          (do
            (index/remove @{:index (workspace :disk-index)} document-uri)
            (when (workspace :scan)
              (put (workspace :scan-changes) document-uri :deleted)))))))
  (each workspace (distinct changed-workspaces)
    (try
      (do
        (def cached
          (index-cache/write (workspace :cache-path) (workspace :uri)
                             (workspace :exclusions) (workspace :disk-index)))
        (unless (workspace :scan) (put workspace :cache-current cached)))
      ([err]
        (logging/warn (string "Could not update workspace index cache: " err)
                      [:index]))))
  (partition-indexes state)
  (reanalyze-open-documents state)
  [:noresponse state])

(defn initialization-uris [params]
  (def folders (get params "workspaceFolders"))
  (cond
    (indexed? folders) (map |(get $ "uri") folders)
    (get params "rootUri") [(get params "rootUri")]
    (get params "rootPath") [(uri/path->file-uri (get params "rootPath"))]
    @[]))

(defn trust-requests [state workspaces]
  (def requests @[])
  (each workspace workspaces
    (when (and (not (workspace :trusted))
               (not (workspace :trust-prompted))
               (workspace :path))
      (put workspace :trust-prompted true)
      (def id (string "janet-lsp/workspaceTrust/" (hash (workspace :uri))))
      (put (state :pending-requests) id
           {:kind :workspace-trust :uri (workspace :uri)})
      (array/push requests
                  {:id id
                   :method "window/showMessageRequest"
                   :params {:type 2
                            :message (string "Trust Janet workspace " (workspace :path)
                                             "? Trusted analysis can execute workspace code.")
                            :actions [{:title "Trust for This Session"}
                                      {:title "Keep Restricted"}]}})))
  requests)

(varfn reanalyze-open-documents [state &opt workspaces]
  (each document (values (state :documents))
    (def owner (server-utils/document-workspace state document))
    (when (or (nil? workspaces) (has-value? workspaces owner))
      (analysis/invalidate document)
      (analysis/refresh document owner (state :position-encoding))))
  state)

(defn on-folders-changed [state params]
  (each folder (get-in params ["event" "removed"] @[])
    (def root-uri (get folder "uri"))
    (when-let [removed (get (state :workspaces) root-uri)]
      (stop-scan removed))
    (put (state :workspaces) root-uri nil)
    (eachp [id pending] (state :pending-requests)
      (when (= root-uri (pending :uri))
        (put (state :pending-requests) id nil))))
  (def added @[])
  (each folder (get-in params ["event" "added"] @[])
    (def root-uri (get folder "uri"))
    (when (uri/file-uri->path root-uri)
      (def configured
        (configure root-uri (state :trusted-workspaces)
                   (state :diagnostic-settings) (state :diagnostic-generation)))
      (load-cache configured)
      (put (state :workspaces) root-uri configured)
      (array/push added configured)))
  (partition-indexes state)
  (reanalyze-open-documents state)
  (def requests (array ;(start-scans state added) ;(trust-requests state added)))
  (if (empty? requests) [:noresponse state] [:requests state requests]))

(defn handle-client-response [message state]
  (def id (get message "id"))
  (when-let [pending (get (state :pending-requests) id)]
    (put (state :pending-requests) id nil)
    (case (pending :kind)
      :progress-create
      (when-let [current (get (state :workspaces) (pending :uri))]
        (when (and (current :scan) (nil? (get message "error")))
          (put current :progress-started true)
          (progress (pending :token)
                    {:kind "begin" :title "Index Janet workspace"})))
      :workspace-trust
      (when (= "Trust for This Session" (get-in message ["result" "title"]))
        (def root-uri (pending :uri))
        (when-let [current (get (state :workspaces) root-uri)]
          (unless (has-value? (state :trusted-workspaces) root-uri)
            (array/push (state :trusted-workspaces) root-uri))
          (def trusted
            (configure root-uri (state :trusted-workspaces)
                       (state :diagnostic-settings) (state :diagnostic-generation)))
          (put trusted :index (current :index))
          (put trusted :disk-index (current :disk-index))
          (put trusted :cache-path (current :cache-path))
          (put trusted :cache-current (current :cache-current))
          (put trusted :exclusions (current :exclusions))
          (merge-into current trusted)
          (reanalyze-open-documents state)))
      :diagnostic-refresh nil))
  state)

(defn on-configuration-changed [state params]
  (def settings (configuration/diagnostics (get params "settings" {})))
  (if (deep= settings (state :diagnostic-settings))
    [:noresponse state]
    (do
      (def generation (inc (state :diagnostic-generation)))
      (put state :diagnostic-settings settings)
      (put state :diagnostic-generation generation)
      (each configured
            (array ;(values (state :workspaces)) ;[(state :standalone-workspace)])
        (put configured :diagnostic-settings settings)
        (put configured :diagnostic-generation generation))
      (reanalyze-open-documents state)
      (cond
        (dyn :push-diagnostics)
        (let [notifications
              (map (fn [document]
                     {:method "textDocument/publishDiagnostics"
                      :params {:uri (document :uri)
                               :version (document :version)
                               :diagnostics (get-in document
                                                    [:analysis :diagnostics])}})
                   (values (state :documents)))]
          [:notifications state notifications])

        (state :diagnostic-refresh-support)
        (let [id (string "janet-lsp/diagnostic/refresh/" generation)]
          (put (state :pending-requests) id {:kind :diagnostic-refresh})
          [:request state {:id id :method "workspace/diagnostic/refresh" :params {}}])

        [:noresponse state]))))
