(import ./analysis)
(import ./document-features)
(import ./index)
(import ./lookup)
(import ./platform)
(import ./request-control)
(import ./server-utils)
(import ./uri)
(import spork/path)

(defn- configured? [state category]
  (not= false (get-in state [:code-lenses category])))

(defn- form-records [document]
  (def content (document :content))
  (catseq [form :in (get-in (document-features/scan content) [:forms] @[])
           :when (and (form :complete) (= 40 (form :open))
                      (not (form :parent)))
           :let [parsed
                 (try (parse (string/slice content (form :start) (form :end)))
                   ([_] nil))]
           :when (and (tuple? parsed) (not (empty? parsed)))]
    {:form form :parsed parsed}))

(defn- metadata? [parsed key]
  (has-value? parsed key))

(defn- test-name [parsed]
  (cond
    (and (= 'deftest (get parsed 0)) (string? (get parsed 1))) (get parsed 1)
    (and (= 'deftest: (get parsed 0)) (string? (get parsed 2))) (get parsed 2)
    nil))

(defn- definition-at-form [record form content]
  (def start (lookup/from-index (form :start) content))
  (first (filter |(server-utils/same-position?
                    start (get-in $ [:range :start]))
                 (record :definitions))))

(defn- data [document kind name range &opt identity]
  {:uri (document :uri) :version (document :version)
   :contentHash (get-in document [:analysis :content-hash])
   :kind kind :name name :range range :identity identity})

(defn lenses [state document]
  (def workspace (server-utils/document-workspace state document))
  (def snapshot (or (analysis/current document workspace)
                    (analysis/refresh document workspace
                                      (state :position-encoding))))
  (def record (snapshot :index))
  (def content (document :content))
  (def result @[])
  (def forms (form-records document))
  (def test-counts
    (frequencies (filter string? (map |(test-name ($ :parsed)) forms))))

  (when (configured? state :references)
    (each definition (record :definitions)
      (when (and (definition :top-level) (not (definition :generated)))
        (array/push result
                    {:range (server-utils/lsp-range
                              state content (definition :selection-range))
                     :data (data document :references (definition :name)
                                 (definition :selection-range)
                                 (definition :identity))}))))

  (when (workspace :trusted)
    (each item forms
      (def parsed (item :parsed))
      (def form (item :form))
      (def byte-range {:start (lookup/from-index (form :start) content)
                       :end (lookup/from-index (form :end) content)})
      (when-let [name (and (configured? state :tests) (test-name parsed)
                           (= 1 (get test-counts (test-name parsed))))]
        (array/push result
                    {:range (server-utils/lsp-range state content byte-range)
                     :data (data document :test name byte-range)}))
      (when-let [definition (definition-at-form record form content)]
        (when (and (= 12 (definition :kind))
                   (= 1 (length (filter |(= (definition :name) ($ :name))
                                        (record :definitions))))
                   (configured? state :flycheck)
                   (metadata? parsed :flycheck))
          (array/push result
                      {:range (server-utils/lsp-range
                                state content (definition :selection-range))
                       :data (data document :flycheck (definition :name)
                                   (definition :selection-range)
                                   (definition :identity))}))
        (when (and (= 12 (definition :kind))
                   (= 1 (length (filter |(= (definition :name) ($ :name))
                                        (record :definitions))))
                   (configured? state :runnable)
                   (or (= "main" (definition :name))
                       (metadata? parsed :entry)))
          (array/push result
                      {:range (server-utils/lsp-range
                                state content (definition :selection-range))
                       :data (data document :run (definition :name)
                                   (definition :selection-range)
                                   (definition :identity))})))))
  (sort-by |[(get-in $ [:range :start :line])
             (get-in $ [:range :start :character])
             (string (get-in $ [:data :kind]))]
           result))

(defn on-code-lens [state params]
  (if-let [document (server-utils/document state params)]
    [:ok state (lenses state document)]
    [:ok state @[]]))

(defn- fresh-document [state lens]
  (when-let [uri (get-in lens ["data" "uri"])
             document (get-in state [:documents uri])]
    (when (and (= (document :version) (get-in lens ["data" "version"]))
               (= (get-in document [:analysis :content-hash])
                  (get-in lens ["data" "contentHash"])))
      document)))

(defn on-resolve [state lens]
  (if-let [document (fresh-document state lens)]
    (let [kind (get-in lens ["data" "kind"])
          name (get-in lens ["data" "name"])
          workspace (server-utils/document-workspace state document)
          resolved
          (case (keyword (string kind))
            :references
            (let [identity (get-in lens ["data" "identity"])
                  definition (index/definition-by-identity workspace identity)
                  sources @{}
                  locations
                  (catseq [reference :in
                           (index/references-by-identity workspace identity)
                           :when (not (and definition
                                           (= (definition :uri) (reference :uri))
                                           (deep= (definition :selection-range)
                                                  (reference :range))))
                           :let [content (request-control/content
                                          state sources (reference :uri))]
                           :when content]
                    {:uri (reference :uri)
                     :range (server-utils/lsp-range state content (reference :range))})
                  references locations]
              (merge lens
                     {:command {:title (string (length references) " reference"
                                               (if (= 1 (length references)) "" "s"))
                                :command "editor.action.showReferences"
                                :arguments [(document :uri)
                                            (get-in lens ["range" "start"])
                                            references]}}))

            :test (and (workspace :trusted)
                       (merge lens {:command {:title "Run test"
                                              :command "janet-lsp.runTest"
                                              :arguments [(document :uri) name
                                                          (get-in lens ["data" "identity"])]}}))
            :flycheck (and (workspace :trusted)
                           (merge lens {:command {:title "Run flycheck"
                                                  :command "janet-lsp.runFlycheck"
                                                  :arguments [(document :uri) name
                                                              (get-in lens ["data" "identity"])]}}))
            :run (and (workspace :trusted)
                      (merge lens {:command {:title "Run definition"
                                             :command "janet-lsp.runDefinition"
                                             :arguments [(document :uri) name
                                                         (get-in lens ["data" "identity"])]}}))
            nil)]
      [:ok state (or resolved lens)])
    [:ok state lens]))

(defn- command-kind [command]
  (case command
    "janet-lsp.runTest" :test
    "janet-lsp.runFlycheck" :flycheck
    "janet-lsp.runDefinition" :run
    nil))

(defn- read-bounded [stream]
  (def limit 1048576)
  (def output @"")
  (var total 0)
  (var chunk (ev/read stream 65536 nil 30))
  (while chunk
    (+= total (length chunk))
    (when (< (length output) limit)
      (buffer/push-string output
                          (string/slice chunk 0
                                        (min (length chunk)
                                             (- limit (length output))))))
    (set chunk (ev/read stream 65536 nil 30)))
  {:text (string output) :truncated (> total limit)})

(defn on-execute-command [state params]
  (def command (get params "command"))
  (def arguments (get params "arguments" @[]))
  (def uri (get arguments 0))
  (def name (get arguments 1))
  (def identity (get arguments 2))
  (def kind (command-kind command))
  (if-let [document (and kind (string? uri) (string? name)
                         (get-in state [:documents uri]))
           workspace (server-utils/document-workspace state document)
           filepath (uri/file-uri->path uri)]
    (if (not (workspace :trusted))
      [:rpc-error state -32600 "Workspace is not trusted" nil]
      (let [evidence
            (some |(and (= kind (get-in $ [:data :kind]))
                        (= name (get-in $ [:data :name]))
                        (or (= :test kind)
                            (= identity (get-in $ [:data :identity]))))
                  (lenses state document))]
        (if (or (nil? evidence)
                (not= (try (slurp filepath) ([_] nil)) (document :content)))
          [:rpc-error state -32602 "Invalid params" "command target is stale"]
          (let [judge (path/join (workspace :path) "jpm_tree/bin/judge")
                expression (string "(dofile " (string/format "%q" filepath)
                                   ")\n(" name ")")
                argv (if (= :test kind)
                       [(if (os/stat judge) judge
                          (or (platform/find-executable "judge") "judge"))
                        "--name-exact" name filepath]
                       [(or (platform/find-executable "janet") "janet")
                        "-e" expression])]
            (var process nil)
            (match (try
                     (ev/with-deadline 30
                       (set process (os/spawn argv :xp {:cwd (workspace :path)
                                                        :out :pipe :err :pipe}))
                       (def [stdout stderr status]
                         (ev/gather (read-bounded (process :out))
                                    (read-bounded (process :err))
                                    (os/proc-wait process)))
                       [:ok {:exitCode status
                             :stdout (stdout :text) :stderr (stderr :text)
                             :stdoutTruncated (stdout :truncated)
                             :stderrTruncated (stderr :truncated)}])
                     ([error]
                      (do (when process (try (os/proc-kill process true) ([_] nil)))
                          [:error error])))
              [:ok result] [:ok state result]
              [:error error]
              [:rpc-error state -32603 "Command failed to start" (string error)])))))
    [:rpc-error state -32602 "Invalid params" "unknown code lens command"]))
