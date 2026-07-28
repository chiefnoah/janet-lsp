(import spork/json)
(import ../libs/fmt)
(import ./utils)
(import ./doc)
(import ./eval)
(import ./logging)
(import ./lookup)
(import ./rpc)
(import ./parser)
(import ./position)
(import ./transport)
(import ./uri)

(import cmd)
(import spork/argparse)
(import spork/path)
(import spork/rpc)

(use judge)

(def version "0.0.12")
(def commit
  (with [proc (os/spawn ["git" "rev-parse" "--short" "HEAD"] :xp {:out :pipe})]
    (let [[out] (ev/gather
                  (ev/read (proc :out) :all)
                  (os/proc-wait proc))]
      (if out (string/trimr out) ""))))

(def jpm-defs (require "../libs/jpm-defs"))

(eachk k jpm-defs
  (match (type k) :symbol (put-in jpm-defs [k :source-map] nil) nil))

(defn run-diagnostics [filepath content encoding workspace]
  (let [items @[]
        [diagnostics env]
        (eval/eval-buffer content
                          (or filepath "untitled.janet")
                          {:trusted (workspace :trusted)
                           :base-env (workspace :env)
                           :unique-paths (workspace :unique-paths)})]

    (logging/dbg (string/format "`eval-buffer` returned: %m" diagnostics) [:evaluation])

    (each res diagnostics
      (match res
        {:location [line col] :message message :severity severity}
        (when-let [lsp-position
                   (position/byte->lsp-position
                     content {:line (max 0 (dec line)) :character col} encoding)]
          (array/push items
                      {:range {:start lsp-position :end lsp-position}
                       :message message
                       :severity severity}))))

    (logging/info (string/format "`run-diagnostics` is returning these errors: %m" items) [:evaluation])
    (logging/dbg (string/format "`run-diagnostics` is returning this eval-context: %m" env) [:evaluation])
    [items env]))

(defn document-key [uri]
  uri)

(defn- path-in-workspace? [filepath root-path]
  (def candidate (if (= :windows (os/which)) (string/ascii-lower filepath) filepath))
  (def root (if (= :windows (os/which)) (string/ascii-lower root-path) root-path))
  (or (= candidate root)
      (string/has-prefix? (string root (if (string/has-suffix? "/" root) "" "/"))
                          candidate)))

(defn workspace-for-path [state filepath]
  (var owner nil)
  (when filepath
    (each workspace (values (state :workspaces))
      (when (and (path-in-workspace? filepath (workspace :path))
                 (or (nil? owner)
                     (> (length (workspace :path)) (length (owner :path)))))
        (set owner workspace))))
  (or owner (state :standalone-workspace)))

(defn document-workspace [state document]
  (workspace-for-path state (document :path)))

(defn request-byte-position [state params content]
  (position/lsp->byte-position content (get params "position")
                               (state :position-encoding)))

(defn on-document-change
  ``
  Handler for the ["textDocument/didChange"](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange) event.
  
  Params contains the new state of the document.
  ``
  [state params]
  (let [client-uri (get-in params ["textDocument" "uri"])
        uri (document-key client-uri)
        document (get-in state [:documents uri])
        version (get-in params ["textDocument" "version"])
        current-version (and document (document :version))]
    (if (or (nil? document)
            (and (number? version)
                 (number? current-version)
                 (<= version current-version)))
      [:noresponse state]
      (let [workspace (document-workspace state document)
            content (get-in params ["contentChanges" 0 "text"])
            [diagnostics env] (run-diagnostics (document :path) content
                                               (state :position-encoding)
                                               workspace)]
        (put document :content content)
        (put document :version version)
        (put document :eval-env env)
        (if (dyn :push-diagnostics)
          (let [message {:method "textDocument/publishDiagnostics"
                         :params {:uri (document :uri)
                                  :version version
                                  :diagnostics diagnostics}}]
            (logging/message message [:diagnostics])
            [:ok state message :notify true])
          [:noresponse state])))))

(defn on-document-close [state params]
  (let [uri (document-key (get-in params ["textDocument" "uri"]))
        document (get-in state [:documents uri])]
    (if (nil? document)
      [:noresponse state]
      (do
        (put (state :documents) uri nil)
        (if (dyn :push-diagnostics)
          (let [message {:method "textDocument/publishDiagnostics"
                         :params {:uri (document :uri)
                                  :diagnostics @[]}}]
            (logging/message message [:diagnostics])
            [:ok state message :notify true])
          [:noresponse state])))))

(defn on-document-diagnostic [state params]
  (let [uri (document-key (get-in params ["textDocument" "uri"]))
        document (get-in state [:documents uri])]
    (if (nil? document)
      [:ok state :null]
      (let [workspace (document-workspace state document)
            [diagnostics env] (run-diagnostics (document :path) (document :content)
                                               (state :position-encoding)
                                               workspace)
            message {:kind "full"
                     :items diagnostics}]
        (put document :eval-env env)
        (logging/message message [:diagnostics])
        [:ok state message]))))

(defn on-document-formatting [state params]
  (let [uri (document-key (get-in params ["textDocument" "uri"]))
        content (get-in state [:documents uri :content])
        new-content (freeze (fmt/format (string/slice content)))]
    (logging/info (string/format "old content: %m" content) [:formatting])
    (logging/info (string/format "new content: %m" new-content) [:formatting])
    (logging/info (string/format "formatting changed something: %m" (not= content new-content)) [:formatting])
    (if (= content new-content)
      (do
        (logging/info "No changes" [:formatting])
        [:ok state :null])
      (do
        (let [message [{:range {:start {:line 0 :character 0}
                                :end (position/document-end content
                                                            (state :position-encoding))}
                        :newText new-content}]]
          (logging/message message [:formatting])
          [:ok state message])))))

(defn on-document-open [state params]
  (let [content (get-in params ["textDocument" "text"])
        client-uri (get-in params ["textDocument" "uri"])
        uri (document-key client-uri)
        version (get-in params ["textDocument" "version"])
        filepath (uri/file-uri->path client-uri)
        workspace (workspace-for-path state filepath)
        [diagnostics env] (run-diagnostics filepath content
                                           (state :position-encoding)
                                           workspace)]
    (put-in state [:documents uri] @{:content content
                                     :version version
                                     :uri client-uri
                                     :path filepath
                                     :eval-env env})
    (logging/info "Document opened" [:open] 1)
    (if (dyn :push-diagnostics)
      (let [message {:method "textDocument/publishDiagnostics"
                     :params {:uri client-uri
                              :version version
                              :diagnostics diagnostics}}]
        (logging/message message [:diagnostics])
        [:ok state message :notify true])
      [:noresponse state])))

(defmacro binding-to-lsp-item
  "Takes a binding and returns a CompletionItem"
  [name eval-env]
  (with-syms [$name $eval-env]
    ~(let [,$name ,name
           ,$eval-env ,eval-env
           s (get-in ,$eval-env [,$name :value] ,$name)]
       (,logging/dbg (string/format "binding-to-lsp-item: s is %m" s) [:completion] 3)
       {:label ,$name :kind
        (case (type s)
          :symbol 12 :boolean 6
          :function 3 :cfunction 3
          :string 6 :buffer 6
          :number 6 :keyword 6
          :core/file 17 :core/peg 6
          :struct 6 :table 6
          :tuple 6 :array 6
          :fiber 6 :nil 6)})))

(defn on-completion [state params]
  (let [uri (document-key (get-in params ["textDocument" "uri"]))
        eval-env (get-in state [:documents uri :eval-env])
        content (get-in state [:documents uri :content])
        location (request-byte-position state params content)
        local-bindings (parser/get-syms-at-loc location content)
        bindings (seq [bind :in (all-bindings eval-env)]
                   (binding-to-lsp-item bind eval-env))
        deduped-bindings (utils/concat-dedup-by-label local-bindings bindings)
        message {:isIncomplete true
                 :items (map |(merge $ {:data {:uri uri}}) deduped-bindings)}]
    (logging/message message [:completion] 1)
    [:ok state message]))

(defn on-completion-item-resolve [state params]
  (def lbl (get params "label"))
  (def document-uri (get-in params ["data" "uri"]))
  (def eval-env (get-in state [:documents document-uri :eval-env]))

  (let [message {:label lbl
                 :documentation
                 {:kind "markdown"
                  :value (doc/my-doc*
                           (symbol lbl)
                           (or eval-env (make-env root-env)))}}]
    (logging/message message [:completion])
    [:ok state message]))

(defn on-document-hover [state params]
  (let [uri (document-key (get-in params ["textDocument" "uri"]))
        content (get-in state [:documents uri :content])
        eval-env (get-in state [:documents uri :eval-env])
        {:line line :character character} (request-byte-position state params content)
        {:word hover-word :range [start end]} (lookup/word-at {:line line :character character} content)
        hover-text (doc/my-doc* (symbol hover-word) eval-env)
        _ (logging/log (string/format "on-document-hover: hover-text is %m" hover-text) [:hover])
        start-position (position/byte->lsp-position
                         content {:line line :character start} (state :position-encoding))
        end-position (position/byte->lsp-position
                       content {:line line :character end} (state :position-encoding))
        message (if (and hover-word hover-text start-position end-position)
                  {:contents {:kind "markdown"
                              :value hover-text}
                   :range {:start start-position :end end-position}}
                  :null)]
    (logging/message message [:hover])
    [:ok state message]))

(defn on-document-signature-help [state params]
  (logging/info (string "on-signature-help state: ") [:signature])
  (logging/info (string/format "%q" state) [:signature])
  (logging/info (string "on-signature-help params: ") [:signature])
  (logging/info (string/format "%q" params) [:signature])
  (let [uri (document-key (get-in params ["textDocument" "uri"]))
        content (get-in state [:documents uri :content])
        eval-env (get-in state [:documents uri :eval-env])
        {:line line :character character} (request-byte-position state params content)
        {:source sexp-text :range [start end]} (lookup/sexp-at {:line line :character character} content)
        function-symbol (or (first (peg/match '(* "(" (any :s) (<- (to " "))) sexp-text)) "none")
        signature (or (doc/get-signature (symbol function-symbol) eval-env) "not found")]
    (case signature
      "not found"
      (do (logging/info "No signature found" [:signature]) [:ok state :null])
      (let [message {:signatures [{:label signature}]}]
        (logging/message message [:signature])
        [:ok state message]))))

(defn find-all-module-files [path &opt search-jpm-tree explicit results]
  (default explicit true)
  (default results @[])
  (case (os/stat path :mode)
    :directory (when (or explicit
                         search-jpm-tree
                         (not= (path/basename path) "jpm_tree"))
                 (each entry (os/dir path)
                   (find-all-module-files (path/join path entry)
                                          search-jpm-tree false results)))
    :file (when (or explicit (not= (path/basename path) "project.janet"))
            (when (or (string/has-suffix? ".janet" path)
                      (string/has-suffix? ".jimage" path)
                      (string/has-suffix? ".so" path))
              (array/push results path))))
  results)

(defn find-unique-paths [paths]
  (->> (seq [found-path :in paths]
         (if (= (path/basename found-path) "init.janet")
           [(path/join (path/dirname found-path)
                       (string ":all:" (path/ext found-path)))
            (path/join (path/dirname found-path) "init.janet")]
           [(path/join (path/dirname found-path)
                       (string ":all:" (path/ext found-path)))]))
       flatten
       distinct))

(defn configure-workspace [root-uri trusted-workspaces]
  (def root-path (uri/file-uri->path root-uri))
  (def trusted (and root-uri root-path (has-value? trusted-workspaces root-uri)))
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
    :trust-prompted false
    :env workspace-env
    :unique-paths unique-paths})

(defn initialization-workspace-uris [params]
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
      (put-in state [:pending-requests id]
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

(defn reanalyze-open-documents [state]
  (each document (values (state :documents))
    (def workspace (document-workspace state document))
    (def [_ env]
      (run-diagnostics (document :path) (document :content)
                       (state :position-encoding) workspace))
    (put document :eval-env env))
  state)

(defn on-initialize
  `` 
  Called by the LSP client to recieve a list of capabilities
  that this server provides so the client knows what it can request.
  ``
  [state params]
  (logging/info (string/format "on-initialize called with these params: %m" params) [:initialize])
  (put state :lifecycle :initialized)
  (def position-encodings (get-in params ["capabilities" "general" "positionEncodings"] @[]))
  (def position-encoding (if (has-value? position-encodings "utf-8") "utf-8" "utf-16"))
  (put state :position-encoding position-encoding)
  (def trusted-workspaces
    (or (get-in params ["initializationOptions" "trustedWorkspaces"])
        (get-in params ["initializationOptions" "janetLsp" "trustedWorkspaces"])
        @[]))
  (put state :trusted-workspaces (array ;trusted-workspaces))
  (each root-uri (initialization-workspace-uris params)
    (when (uri/file-uri->path root-uri)
      (put-in state [:workspaces root-uri]
              (configure-workspace root-uri trusted-workspaces))))
  (if-let [diagnostic? (get-in params ["capabilities" "textDocument" "diagnostic"])]
    (setdyn :push-diagnostics false)
    (setdyn :push-diagnostics true))

  (let [message {:capabilities {:positionEncoding position-encoding
                                :completionProvider {:resolveProvider true}
                                :textDocumentSync {:openClose true
                                                   :change 1 # send the Full document https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentSyncKind
}
                                :diagnosticProvider {:interFileDependencies true
                                                     :workspaceDiagnostics false}
                                :hoverProvider true
                                :signatureHelpProvider {:triggerCharacters [" "]}
                                :documentFormattingProvider true
                                :definitionProvider true
                                :workspace {:workspaceFolders
                                            {:supported true
                                             :changeNotifications true}}}
                 :serverInfo {:name "janet-lsp"
                              :version version
                              :commit commit}}]
    (logging/message message [:initialize])
    [:ok state message]))

(defn on-initialized [state]
  (def requests (trust-requests state (values (state :workspaces))))
  (if (empty? requests)
    [:noresponse state]
    [:requests state requests]))

(defn on-workspace-folders-changed [state params]
  (each folder (get-in params ["event" "removed"] @[])
    (def root-uri (get folder "uri"))
    (put (state :workspaces) root-uri nil)
    (eachp [id pending] (state :pending-requests)
      (when (= root-uri (pending :uri))
        (put (state :pending-requests) id nil))))
  (def added @[])
  (each folder (get-in params ["event" "added"] @[])
    (def root-uri (get folder "uri"))
    (when (uri/file-uri->path root-uri)
      (def workspace (configure-workspace root-uri (state :trusted-workspaces)))
      (put-in state [:workspaces root-uri] workspace)
      (array/push added workspace)))
  (reanalyze-open-documents state)
  (def requests (trust-requests state added))
  (if (empty? requests)
    [:noresponse state]
    [:requests state requests]))

(defn handle-client-response [message state]
  (def id (get message "id"))
  (def pending (get-in state [:pending-requests id]))
  (when pending
    (put (state :pending-requests) id nil)
    (when (and (= :workspace-trust (pending :kind))
               (= "Trust for This Session" (get-in message ["result" "title"])))
      (def root-uri (pending :uri))
      (when (get-in state [:workspaces root-uri])
        (unless (has-value? (state :trusted-workspaces) root-uri)
          (array/push (state :trusted-workspaces) root-uri))
        (put-in state [:workspaces root-uri]
                (configure-workspace root-uri (state :trusted-workspaces)))
        (reanalyze-open-documents state))))
  state)

(defn on-shutdown
  ``
  Called by the LSP client to request that the server shut down.
  ``
  [state params]
  (put state :lifecycle :shutdown)
  (put state :pending-requests @{})
  (logging/info "Shutting down" [:shutdown])
  [:ok state :null])

(defn on-exit
  ``
  Called by the LSP client to request that the server process exit.
  ``
  [state params]
  (logging/info "Exiting" [:exit])
  [:exit (if (= :shutdown (state :lifecycle)) 0 1)])

(defn on-janet-serverinfo
  ``
  Called by the LSP client to request information about the server.
  ``
  [state params]
  (let [message {:serverInfo {:name "janet-lsp"
                              :version version
                              :commit commit}}]
    (logging/message message [:info])
    [:ok state message]))

(defn on-document-definition
  ``
  Called by the LSP client to request the location of a symbol's definition.
  ``
  [state params]
  (let [request-uri (document-key (get-in params ["textDocument" "uri"]))
        content (get-in state [:documents request-uri :content])
        eval-env (get-in state [:documents request-uri :eval-env])
        {:line line :character character} (request-byte-position state params content)
        {:word define-word :range [start end]} (lookup/word-at {:line line :character character} content)]
    (logging/info (string/format ``
                                -------------------------
                                uri is: %s
                                content length is: %d
                                line is: %d
                                character is: %d
                                define word is: %s
                                start is: %d
                                end is: %d
                                -------------------------
                                ``
                                 request-uri (length content) line character define-word start end) [:definition])
    (logging/info (string/format "symbol is: %s" (symbol define-word)) [:definition])
    (logging/dbg (string/format "eval-env is: %m" eval-env) [:definition])
    (logging/info (string/format "symbol lookup is: %m" (get eval-env (symbol define-word) nil)) [:definition])
    (logging/info (string/format "`:source-map` is: %m" (get (get eval-env (symbol define-word) nil) :source-map nil)) [:definition])
    (if-let [symbol-lookup (get eval-env (symbol define-word) nil)
             [uri line col] (get symbol-lookup :source-map nil)
             target-path (path/abspath uri)
             found (os/stat target-path)
             target-content (slurp target-path)
             target-position (position/byte->lsp-position
                               target-content
                               {:line (max 0 (dec line)) :character col}
                               (state :position-encoding))
             filepath (uri/path->file-uri target-path)
             message {:uri filepath
                      :range {:start target-position
                              :end target-position}}]
      (do
        (logging/message message [:definition])
        [:ok state message])
      (do
        (logging/info "Couldn't find definition" [:definition])
        [:ok state :null]))))

(defn on-set-trace [state params]
  (logging/info (string/format "on-set-trace: %m" params) [:settrace])
  (case (params "value")
    "off" nil
    "messages" nil
    "verbose" nil)
  [:noresponse state])

(defn on-janet-tell-joke [state params]
  (let [message {:question "What's brown and sticky?"
                 :answer "A stick!"}]
    (logging/message message [:joke])
    [:ok state message]))

(defn on-enable-debug [state params]
  (let [message {:message "Enabled :debug"}]
    (setdyn :debug true)
    (try (spit "janetlsp.log" "")
      ([_] (logging/err "Tried to write to janetlsp.log, but couldn't" [:core])))
    (logging/message message [:debug])
    [:ok state message]))

(defn on-disable-debug [state params]
  (let [message {:message "Disabled :debug"}]
    (setdyn :debug false)
    (setdyn :log-level 2)
    (logging/message message [:debug])
    [:ok state message]))

(defn do-set-log-level [state params kind]
  (let [new-level-string (params "level")
        new-level ({"off" 0 "messages" 1 "verbose" 2 "veryverbose" 3} new-level-string)
        message {:message (string/format "Set %s to %s" kind new-level-string)}]
    (logging/message message [:loglevel])
    (setdyn kind new-level)
    [:noresponse state]))

(defmacro on-set-log-level [state params]
  ~(,do-set-log-level ,state ,params :log-level))

(defmacro on-set-file-log-level [state params]
  ~(,do-set-log-level ,state ,params :log-to-file-level))

(def open-document-request-methods
  {"textDocument/completion" true
   "textDocument/diagnostic" true
   "textDocument/formatting" true
   "textDocument/hover" true
   "textDocument/signatureHelp" true
   "textDocument/definition" true})

(def position-request-methods
  {"textDocument/completion" true
   "textDocument/hover" true
   "textDocument/signatureHelp" true
   "textDocument/definition" true})

(defn handle-message [message state]
  (let [id (get message "id")
        method (get message "method")
        params (get message "params")
        document-uri (get-in params ["textDocument" "uri"])
        document (get-in state [:documents (document-key document-uri)])]
    (logging/info (string/format "handle-message received method request: `%s`" method) [:core] 0 id)
    (if (or (and (get open-document-request-methods method) (nil? document))
            (and (get position-request-methods method)
                 (nil? (request-byte-position state params (document :content)))))
      [:ok state :null]
      (case method
      "initialize" (on-initialize state params)
      "initialized" (on-initialized state)
      "textDocument/didOpen" (on-document-open state params)
      "textDocument/didChange" (on-document-change state params)
      "textDocument/didClose" (on-document-close state params)
      "textDocument/completion" (on-completion state params)
      "completionItem/resolve" (on-completion-item-resolve state params)
      "textDocument/diagnostic" (on-document-diagnostic state params)
      "textDocument/formatting" (on-document-formatting state params)
      "textDocument/hover" (on-document-hover state params)
      "textDocument/signatureHelp" (on-document-signature-help state params)
      # "textDocument/references" (on-document-references state params) TODO: Implement this? See src/lsp/api.ts:103
      # "textDocument/documentSymbol" (on-document-symbols state params) TODO: Implement this? See src/lsp/api.ts:121
      "textDocument/definition" (on-document-definition state params)
      "workspace/didChangeWorkspaceFolders" (on-workspace-folders-changed state params)
      "janet/serverInfo" (on-janet-serverinfo state params)
      "janet/tellJoke" (on-janet-tell-joke state params)
      "enableDebug" (on-enable-debug state params)
      "disableDebug" (on-disable-debug state params)
      "setLogLevel" (on-set-log-level state params)
      "setLogToFileLevel" (on-set-file-log-level state params)
      "shutdown" (on-shutdown state params)
      "exit" (on-exit state params)
      "$/setTrace" (on-set-trace state params)
      (do
        (logging/warn (string/format "Received unrecognized RPC: %m" method) [:handle])
        [:method-not-found state])))))

(defn write-response [file response]
  (transport/write-frame file response))

(defn read-message []
  (when-let [input (transport/read-frame stdin)]
    (logging/info (string/format "received json rpc: %s" input) [:rpc :priority])
    (try [:ok (json/decode input)]
      ([err] [:parse-error err]))))

(defn write-rpc-error [id code message &opt data]
  (write-response stdout (rpc/error-response id code message data)))

(defn lifecycle-action [message state]
  (def lifecycle (state :lifecycle))
  (def method (get message "method"))
  (def notification? (rpc/notification? message))
  (case lifecycle
    :uninitialized
    (cond
      (= method "initialize") (if notification?
                                [-32600 "Invalid Request" "initialize must be a request"]
                                nil)
      (= method "exit") nil
      notification? :ignore
      [-32002 "Server not initialized" nil])

    :initialized
    (cond
      (= method "initialize") [-32600 "Invalid Request" "initialize may only be sent once"]
      (= method "initialized") (if notification?
                                 nil
                                 [-32600 "Invalid Request" "initialized must be a notification"])
      (= method "shutdown") (if notification?
                              :ignore
                              nil)
      nil)

    :shutdown
    (cond
      (= method "exit") nil
      notification? :ignore
      [-32600 "Invalid Request" "server has shut down"])))

(defn dispatch-message [message state]
  (def id (rpc/message-id message))
  (def notification? (rpc/notification? message))
  (if (rpc/response? message)
    (handle-client-response message state)
  (if-let [[code error-message data] (rpc/validate-message message)]
    (do
      (write-rpc-error id code error-message data)
      state)
    (match (lifecycle-action message state)
      :ignore state
      [code error-message data]
      (do
        (write-rpc-error id code error-message data)
        state)
      nil
      (match (try (handle-message message state) ([err fib] [:error state err fib]))
        [:ok new-state response :notify true]
        (do
          (write-response stdout (rpc/notification response))
          new-state)

        [:ok new-state response]
        (do
          (unless notification?
            (write-response stdout (rpc/success-response id response)))
          new-state)

        [:request new-state request]
        (do
          (write-response stdout
                          (rpc/request (request :id)
                                       (request :method)
                                       (request :params)))
          new-state)

        [:requests new-state requests]
        (do
          (each request requests
            (write-response stdout
                            (rpc/request (request :id)
                                         (request :method)
                                         (request :params))))
          new-state)

        [:noresponse new-state] new-state

        [:method-not-found new-state]
        (do
          (unless notification?
            (write-rpc-error id -32601 "Method not found"))
          new-state)

        [:error new-state err fib]
        (do
          (logging/err (string/format "%m" err) [:core])
          (debug/stacktrace fib err "")
          (unless notification?
            (write-rpc-error id -32603 "Internal error"))
          new-state)

        [:exit status] [:exit status])))))

(defn message-loop [&named state]
  (logging/info "Loop enter" [:core] 1)
  (logging/dbg (string/format "current state is: %m" state) [:priority])
  (match (read-message)
    nil (do (file/flush stdout) (os/exit 0))
    [:parse-error err]
    (do
      (logging/warn (string/format "Invalid JSON: %s" err) [:rpc])
      (write-rpc-error :null -32700 "Parse error")
      (message-loop :state state))
    [:ok message]
    (do
      (logging/dbg (string/format "got: %q" message) [:core])
      (match (dispatch-message message state)
        [:exit status] (do (file/flush stdout) (os/exit status))
        new-state (message-loop :state new-state)))))

(defn start-language-server []
  (logging/info (string "Starting LSP " version "-" commit) [:core])
  (when (dyn :debug)
    (try (spit "janetlsp.log" "")
      ([_] (logging/err "Tried to write to janetlsp.log txt, but couldn't" [:core]))))

  (merge-module root-env jpm-defs nil true)

  (message-loop :state @{:documents @{}
                         :lifecycle :uninitialized
                         :position-encoding "utf-16"
                         :pending-requests @{}
                         :trusted-workspaces @[]
                         :workspaces @{}
                         :standalone-workspace @{:uri nil
                                                 :path nil
                                                 :trusted false
                                                 :env (make-env root-env)
                                                 :unique-paths @[]}}))

(defn start-debug-console []
  (def host "127.0.0.1")
  (def port (if ((dyn :opts) :port) (string ((dyn :opts) :port)) "8037"))

  (print "Janet LSP Debug Console v" version "-" commit)
  (print (string/format "Listening on %s:%s" host port))
  (print "Awaiting reports from running LSP...")

  (var linecount 0)

  (rpc/server
    {:print (fn [self x]
              (print (string/format "server:%d:> %s" linecount x))
              (file/flush stdout)
              (+= linecount 1))}
    host port))

(defn main [& args]

  (def parsed-args (cmd/args))

  (when (or (has-value? parsed-args "--version")
            (has-value? parsed-args "-v"))
    (print "Janet LSP v" version "-" commit)
    (os/exit 0))

  (cmd/run
    (cmd/fn
      "A Language Server (LSP) for the Janet Programming Language."
      [[--dont-search-jpm-tree -j] (flag) "Whether to search `jpm_tree` for modules."
       --stdio (flag) "Use STDIO."
       [--debug -d] (flag) "Print debug messages."
       [--log-level -l] (optional :int++ 1) "What level of logging to display. Defaults to 1."
       [--log-to-file-level -f] (optional :int++ 2) "What level of logging to write to the log file. Defaults to 2."
       [--log-category -L] (tuple :string) "Enable logging by category. For multiple categories, repeat the flag."
       [--console -c] (flag) "Start a debug console instead of starting the Language Server."
       [--debug-port -p] (optional :int++) "What port to start or connect to the debug console on. Defaults to 8037."]

      (default stdio true)
      (default debug-port 8037)

      (def opts
        {:dont-search-jpm-tree dont-search-jpm-tree
         :stdio stdio
         :console console
         :debug-port debug-port})

      (setdyn :opts opts)
      (when debug (setdyn :debug true)) #(setdyn :debug true)
      (setdyn :log-level log-level) #(setdyn :log-level 2)
      (setdyn :log-to-file-level log-to-file-level) #(setdyn :log-level 3)
      (setdyn :log-categories @[:core ;(map keyword log-category)]) #(setdyn :log-categories [:core :priority :loglevel])
      (setdyn :out stderr)
      (put root-env :out stderr)

      (if console
        (start-debug-console)
        (start-language-server)))
    parsed-args))
