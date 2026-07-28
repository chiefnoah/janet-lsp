(import ./index)
(import ./logging)
(import ./server-meta)
(import ./uri)
(import ./workspace)

(defn initial-state []
  @{:documents @{}
    :lifecycle :uninitialized
    :position-encoding "utf-16"
    :trace "off"
    :pending-requests @{}
    :cancelled-requests @{}
    :trusted-workspaces @[]
    :workspaces @{}
    :standalone-workspace @{:uri nil
                            :path nil
                            :trusted false
                            :index @{}
                            :exclusions index/default-exclusions
                            :env (make-env root-env)
                            :unique-paths @[]}})

(defn on-initialize [state params]
  (put state :lifecycle :initialized)
  (def offered-encodings
    (get-in params ["capabilities" "general" "positionEncodings"] @[]))
  (def encoding (if (has-value? offered-encodings "utf-8") "utf-8" "utf-16"))
  (put state :position-encoding encoding)
  (put state :work-done-progress
       (not (not (get-in params ["capabilities" "window" "workDoneProgress"]))))
  (put state :inlay-parameter-hints
       (not= false
             (get-in params
                     ["initializationOptions" "inlayHints" "parameterNames"])))
  (def trusted
    (or (get-in params ["initializationOptions" "trustedWorkspaces"])
        (get-in params ["initializationOptions" "janetLsp" "trustedWorkspaces"])
        @[]))
  (put state :trusted-workspaces (array ;trusted))
  (each root-uri (workspace/initialization-uris params)
    (when (uri/file-uri->path root-uri)
      (put (state :workspaces) root-uri (workspace/configure root-uri trusted))))
  (def configured-exclusions
    (get-in params ["initializationOptions" "excludedDirectories"] @[]))
  (each configured (values (state :workspaces))
    (put configured :exclusions
         (distinct (array ;index/default-exclusions ;configured-exclusions))))
  (setdyn :push-diagnostics
          (nil? (get-in params ["capabilities" "textDocument" "diagnostic"])))
  (def result
    {:capabilities
     {:positionEncoding encoding
      :completionProvider {:resolveProvider true}
      :textDocumentSync {:openClose true :change 1}
      :diagnosticProvider {:interFileDependencies true :workspaceDiagnostics false}
      :hoverProvider true
      :signatureHelpProvider {:triggerCharacters ["(" " "]
                              :retriggerCharacters [" "]}
      :documentFormattingProvider true
      :definitionProvider true
      :documentSymbolProvider true
      :workspaceSymbolProvider true
      :referencesProvider true
      :renameProvider {:prepareProvider true}
      :semanticTokensProvider
      {:legend {:tokenTypes server-meta/semantic-token-types
                :tokenModifiers server-meta/semantic-token-modifiers}
       :full true}
      :codeActionProvider {:codeActionKinds ["quickfix"]}
      :inlayHintProvider (state :inlay-parameter-hints)
      :workspace {:workspaceFolders {:supported true :changeNotifications true}}}
     :serverInfo (server-meta/server-info)})
  (logging/message result [:initialize])
  [:ok state result])

(defn on-initialized [state]
  (def requests
    (array ;(workspace/start-scans state)
           ;(workspace/trust-requests state (values (state :workspaces)))))
  (if (empty? requests) [:noresponse state] [:requests state requests]))

(defn on-shutdown [state]
  (put state :lifecycle :shutdown)
  (put state :pending-requests @{})
  (workspace/stop-scans state)
  (logging/info "Shutting down" [:shutdown])
  [:ok state :null])

(defn on-exit [state]
  (logging/info "Exiting" [:exit])
  [:exit (if (= :shutdown (state :lifecycle)) 0 1)])

(defn on-server-info [state]
  (def result {:serverInfo (server-meta/server-info) :trace (state :trace)})
  (logging/message result [:info])
  [:ok state result])
