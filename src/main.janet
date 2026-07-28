(import spork/json)
(import cmd)
(import spork/rpc :as debug-rpc)

(import ./documents)
(import ./editor-features)
(import ./lifecycle)
(import ./logging)
(import ./navigation)
(import ./rpc)
(import ./server-meta)
(import ./server-utils)
(import ./transport)
(import ./workspace)

(def jpm-defs (require "../libs/jpm-defs"))

(eachk key jpm-defs
  (when (= :symbol (type key))
    (put-in jpm-defs [key :source-map] nil)))

(defn on-set-trace [state params]
  (case (get params "value")
    "off" (put state :trace "off")
    "messages" (put state :trace "messages")
    "verbose" (put state :trace "verbose")
    (logging/warn (string/format "Ignoring invalid trace value: %m"
                                 (get params "value"))
                  [:settrace]))
  [:noresponse state])

(defn on-cancel-request [state params]
  (def id (get params "id"))
  (when (rpc/valid-id? id)
    (put (state :cancelled-requests) id true))
  [:noresponse state])

(defn on-enable-debug [state]
  (setdyn :debug true)
  (try (spit "janetlsp.log" "")
    ([_] (logging/err "Could not write janetlsp.log" [:core])))
  [:ok state {:message "Enabled :debug"}])

(defn on-disable-debug [state]
  (setdyn :debug false)
  (setdyn :log-level 2)
  [:ok state {:message "Disabled :debug"}])

(defn on-set-log-level [state params kind]
  (def name (get params "level"))
  (if-let [level ({"off" 0 "messages" 1 "verbose" 2 "veryverbose" 3} name)]
    (do
      (setdyn kind level)
      [:ok state {:message (string/format "Set %s to %s" kind name)}])
    [:rpc-error state -32602 "Invalid params"
     "level must be off, messages, verbose, or veryverbose"]))

(def open-document-methods
  {"textDocument/completion" true
   "textDocument/diagnostic" true
   "textDocument/formatting" true
   "textDocument/hover" true
   "textDocument/signatureHelp" true
   "textDocument/definition" true
   "textDocument/references" true
   "textDocument/prepareRename" true
   "textDocument/rename" true
   "textDocument/semanticTokens/full" true
   "textDocument/codeAction" true
   "textDocument/inlayHint" true
   "textDocument/documentSymbol" true})

(def position-methods
  {"textDocument/completion" true
   "textDocument/hover" true
   "textDocument/signatureHelp" true
   "textDocument/definition" true
   "textDocument/references" true
   "textDocument/prepareRename" true
   "textDocument/rename" true})

(defn handle-message [message state]
  (let [method (get message "method")
        params (get message "params")
        document (server-utils/document state params)]
    (logging/info (string/format "handling `%s`" method) [:core] 0 (get message "id"))
    (if (or (and (get open-document-methods method) (nil? document))
            (and (get position-methods method)
                 (nil? (server-utils/request-byte-position
                         state params (document :content)))))
      [:ok state :null]
      (case method
        "initialize" (lifecycle/on-initialize state params)
        "initialized" (lifecycle/on-initialized state)
        "shutdown" (lifecycle/on-shutdown state)
        "exit" (lifecycle/on-exit state)

        "textDocument/didOpen" (documents/on-open state params)
        "textDocument/didChange" (documents/on-change state params)
        "textDocument/didClose" (documents/on-close state params)
        "textDocument/diagnostic" (documents/on-diagnostic state params)
        "textDocument/formatting" (documents/on-formatting state params)

        "textDocument/completion" (editor-features/on-completion state params)
        "completionItem/resolve" (editor-features/on-completion-resolve state params)
        "textDocument/hover" (editor-features/on-hover state params)
        "textDocument/signatureHelp" (editor-features/on-signature-help state params)
        "textDocument/semanticTokens/full"
        (editor-features/on-semantic-tokens-full state params)
        "textDocument/codeAction" (editor-features/on-code-action state params)
        "textDocument/inlayHint" (editor-features/on-inlay-hint state params)

        "textDocument/definition" (navigation/on-definition state params)
        "textDocument/documentSymbol" (navigation/on-document-symbols state params)
        "workspace/symbol" (navigation/on-workspace-symbols state params)
        "textDocument/references" (navigation/on-references state params)
        "textDocument/prepareRename" (navigation/on-prepare-rename state params)
        "textDocument/rename" (navigation/on-rename state params)

        "workspace/didChangeWorkspaceFolders" (workspace/on-folders-changed state params)
        "workspace/didChangeWatchedFiles" (workspace/on-watched-files-changed state params)
        "window/workDoneProgress/cancel" (workspace/cancel-scan state params)
        "$/cancelRequest" (on-cancel-request state params)
        "$/setTrace" (on-set-trace state params)

        "janet/serverInfo" (lifecycle/on-server-info state)
        "janet/tellJoke"
        [:ok state {:question "What's brown and sticky?" :answer "A stick!"}]
        "enableDebug" (on-enable-debug state)
        "disableDebug" (on-disable-debug state)
        "setLogLevel" (on-set-log-level state params :log-level)
        "setLogToFileLevel" (on-set-log-level state params :log-to-file-level)
        [:method-not-found state]))))

(defn read-message []
  (when-let [input (transport/read-frame stdin)]
    (logging/info (string/format "received json rpc: %s" input) [:rpc :priority])
    (try [:ok (json/decode input)]
      ([err] [:parse-error err]))))

(defn- write-message [message]
  (transport/write-frame stdout message))

(defn- write-error [id code message &opt data]
  (write-message (rpc/error-response id code message data)))

(defn- write-request [request]
  (write-message
    (rpc/request (request :id) (request :method) (request :params))))

(defn lifecycle-action [message state]
  (def method (get message "method"))
  (def notification? (rpc/notification? message))
  (case (state :lifecycle)
    :uninitialized
    (cond
      (= method "initialize")
      (if notification? [-32600 "Invalid Request" "initialize must be a request"] nil)
      (= method "exit") nil
      notification? :ignore
      [-32002 "Server not initialized" nil])

    :initialized
    (cond
      (= method "initialize") [-32600 "Invalid Request" "initialize may only be sent once"]
      (= method "initialized")
      (if notification? nil [-32600 "Invalid Request" "initialized must be a notification"])
      (= method "shutdown") (if notification? :ignore nil)
      nil)

    :shutdown
    (cond
      (= method "exit") nil
      notification? :ignore
      [-32600 "Invalid Request" "server has shut down"])))

(defn- emit-handler-result [result id notification?]
  (match result
    [:ok state response :notify true]
    (do (write-message (rpc/notification response)) state)

    [:ok state response]
    (do
      (unless notification? (write-message (rpc/success-response id response)))
      state)

    [:request state request]
    (do (write-request request) state)

    [:requests state requests]
    (do (each request requests (write-request request)) state)

    [:rpc-error state code message data]
    (do (unless notification? (write-error id code message data)) state)

    [:noresponse state] state

    [:method-not-found state]
    (do (unless notification? (write-error id -32601 "Method not found")) state)

    [:error state err fiber]
    (do
      (logging/err (string/format "%m" err) [:core])
      (debug/stacktrace fiber err "")
      (unless notification? (write-error id -32603 "Internal error"))
      state)

    [:exit status] [:exit status]))

(defn dispatch-message [message state]
  (workspace/refresh-scans state)
  (def id (rpc/message-id message))
  (def notification? (rpc/notification? message))
  (cond
    (rpc/response? message)
    (workspace/handle-client-response message state)

    (if-let [[code error-message data] (rpc/validate-message message)]
      (do (write-error id code error-message data) true))
    state

    (and (not notification?) (has-key? (state :cancelled-requests) id))
    (do
      (put (state :cancelled-requests) id nil)
      (write-error id -32800 "Request cancelled")
      state)

    (match (lifecycle-action message state)
      :ignore state
      [code error-message data]
      (do (write-error id code error-message data) state)
      nil
      (emit-handler-result
        (try (handle-message message state)
          ([err fiber] [:error state err fiber]))
        id notification?))))

(defn message-loop [state]
  (match (read-message)
    nil (do (file/flush stdout) (os/exit 0))
    [:parse-error err]
    (do
      (logging/warn (string/format "Invalid JSON: %s" err) [:rpc])
      (write-error :null -32700 "Parse error")
      (message-loop state))
    [:ok message]
    (match (dispatch-message message state)
      [:exit status] (do (file/flush stdout) (os/exit status))
      next-state (message-loop next-state))))

(defn start-language-server []
  (logging/info (string "Starting LSP " server-meta/version "-" server-meta/commit) [:core])
  (when (dyn :debug)
    (try (spit "janetlsp.log" "")
      ([_] (logging/err "Could not write janetlsp.log" [:core]))))
  (merge-module root-env jpm-defs nil true)
  (message-loop (lifecycle/initial-state)))

(defn start-debug-console []
  (def host "127.0.0.1")
  (def port (logging/debug-port (dyn :opts)))
  (print "Janet LSP Debug Console v" server-meta/version "-" server-meta/commit)
  (print (string/format "Listening on %s:%s" host port))
  (print "Awaiting reports from running LSP...")
  (var line-count 0)
  (debug-rpc/server
    {:print (fn [self value]
              (print (string/format "server:%d:> %s" line-count value))
              (file/flush stdout)
              (+= line-count 1))}
    host port))

(defn main [& args]
  (def parsed-args (cmd/args))
  (when (or (has-value? parsed-args "--version")
            (has-value? parsed-args "-v"))
    (print "Janet LSP v" server-meta/version "-" server-meta/commit)
    (os/exit 0))
  (cmd/run
    (cmd/fn
      "A Language Server (LSP) for the Janet Programming Language."
      [[--dont-search-jpm-tree -j] (flag) "Whether to search `jpm_tree` for modules."
       --stdio (flag) "Use STDIO."
       [--debug -d] (flag) "Print debug messages."
       [--log-level -l] (optional :int++ 1) "What level of logging to display."
       [--log-to-file-level -f] (optional :int++ 2) "What level of file logging to use."
       [--log-category -L] (tuple :string) "Enable a logging category; repeat as needed."
       [--console -c] (flag) "Start a debug console instead of the language server."
       [--debug-port -p] (optional :int++) "Debug console port. Defaults to 8037."]
      (default stdio true)
      (default debug-port 8037)
      (setdyn :opts {:dont-search-jpm-tree dont-search-jpm-tree
                     :stdio stdio
                     :console console
                     :debug-port debug-port})
      (when debug (setdyn :debug true))
      (setdyn :log-level log-level)
      (setdyn :log-to-file-level log-to-file-level)
      (setdyn :log-categories @[:core ;(map keyword log-category)])
      (setdyn :out stderr)
      (put root-env :out stderr)
      (if console (start-debug-console) (start-language-server)))
    parsed-args))
