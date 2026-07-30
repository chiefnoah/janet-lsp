(import spork/path)
(import ../src/index)
(import ../src/index-cache)
(import ../src/uri)

(use judge ./support/lsp-client)

(deftest-type with-process
  :setup start-lsp
  :reset (fn [cursor]
           (exit-lsp cursor)
           (merge-into cursor (start-lsp)))
  :teardown exit-lsp)

(deftest-type with-process-open
  :setup (fn []
           (def cursor (start-lsp))
           (put cursor :open (open-document cursor))
           cursor)
  :reset (fn [cursor]
           (exit-lsp cursor)
           (def next (start-lsp))
           (put next :open (open-document next))
           (merge-into cursor next))
  :teardown exit-lsp)

(deftest: with-process "initialize and server info" [cursor]
  (def initialize (cursor :initialize))
  (test (get-in initialize [:result :serverInfo :name]) "janet-lsp")
  (test (get-in initialize [:result :capabilities :positionEncoding]) "utf-16")
  (test (get-in initialize [:result :capabilities :completionProvider :resolveProvider]) true)
  (test (get-in initialize [:result :capabilities :completionProvider :triggerCharacters])
        @["/" ":" "." "\""])
  (test (get-in initialize
                [:result :capabilities :diagnosticProvider :workspaceDiagnostics])
        true)
  (test (get-in initialize [:result :capabilities :documentHighlightProvider]) true)
  (test (get-in initialize [:result :capabilities :foldingRangeProvider]) true)
  (test (get-in initialize [:result :capabilities :selectionRangeProvider]) true)
  (test (get-in initialize [:result :capabilities :callHierarchyProvider]) true)
  (test (get-in initialize [:result :capabilities :typeDefinitionProvider]) true)
  (test (get-in initialize [:result :capabilities :implementationProvider]) true)
  (test (get-in initialize [:result :capabilities :documentRangeFormattingProvider]) true)
  (test (get-in initialize
                [:result :capabilities :documentOnTypeFormattingProvider
                 :firstTriggerCharacter])
        ")")
  (test (get-in initialize
                [:result :capabilities :documentOnTypeFormattingProvider
                 :moreTriggerCharacter])
        @["]" "}"])
  (test (get-in initialize [:result :capabilities :codeActionProvider
                            :codeActionKinds])
        @["quickfix" "source.sortImports" "source.organizeImports"])
  (test (get-in initialize
                [:result :capabilities :documentLinkProvider :resolveProvider])
        false)
  (test (get-in (request cursor 1 "janet/serverInfo") [:result :serverInfo :name])
        "janet-lsp"))

(deftest: with-process "validate logging and trace controls" [cursor]
  (test (get-in (request cursor 93 "setLogLevel" {:level "verbose"})
                [:result :message])
        "Set log-level to verbose")
  (test (get-in (request cursor 94 "setLogLevel" {:level "loud"})
                [:error :code])
        -32602)
  (test (get-in (request cursor 95 "setLogToFileLevel" {}) [:error :code]) -32602)
  (notify cursor "$/setTrace" {:value "verbose"})
  (test (get-in (request cursor 96 "janet/serverInfo") [:result :trace]) "verbose")
  (notify cursor "$/setTrace" {:value "invalid"})
  (test (get-in (request cursor 97 "janet/serverInfo") [:result :trace]) "verbose"))

(deftest "nondefault debug ports preserve stdio framing"
  (def port (+ 18000 (% (os/getpid) 1000)))
  (def port-arg (string "--debug-port=" port))
  (def console
    (os/spawn [(dyn :executable) "./src/main.janet" "--console" port-arg]
              :p {:out :pipe}))
  (ev/sleep 0.2)
  (def process
    (os/spawn [(dyn :executable) "./src/main.janet" "--debug" port-arg
               "--dont-search-jpm-tree"]
              :p {:in :pipe :out :pipe}))
  (def cursor @{:process process :to-lsp (process :in) :from-lsp (process :out)})
  (test (get-in
          (request cursor 98 "initialize"
                   {:rootUri workspace-uri :capabilities {}})
          [:result :serverInfo :name])
        "janet-lsp")
  (request cursor 99 "shutdown")
  (notify cursor "exit")
  (wait-process process)
  (os/proc-kill console true))

(deftest "workspace index scans report progress without blocking initialization"
  (def root (temp-directory "janet-lsp-index-progress"))
  (spit (path/join root "main.janet") "(def indexed-value 1)\n")
  (def root-uri (uri/path->file-uri root))
  (def cursor (spawn-lsp))
  (def initialized
    (request cursor 100 "initialize"
             {:rootUri root-uri
              :capabilities {:window {:workDoneProgress true}
                             :textDocument {:diagnostic {}}}}))
  (test (get-in initialized [:id]) 100)
  (notify cursor "initialized")
  (def create (read-output cursor))
  (test (get-in create [:method]) "window/workDoneProgress/create")
  (def trust (read-output cursor))
  (respond cursor (get-in create [:id]) :null)
  (def begin (read-output cursor))
  (test (get-in begin [:method]) "$/progress")
  (test (get-in begin [:params :value :kind]) "begin")
  (respond cursor (get-in trust [:id]) {:title "Keep Restricted"})
  (ev/sleep 0.2)
  (write-output cursor {:jsonrpc "2.0" :id 101 :method "janet/serverInfo" :params {}})
  (def ended (read-output cursor))
  (test (get-in ended [:method]) "$/progress")
  (test (get-in ended [:params :value :kind]) "end")
  (test (get-in (read-output cursor) [:id]) 101)
  (exit-lsp cursor)
  (remove-tree root))

(deftest "workspace index scans run without progress support"
  (def root (temp-directory "janet-lsp-index-no-progress"))
  (spit (path/join root "main.janet") "(def indexed-without-progress 1)\n")
  (def root-uri (uri/path->file-uri root))
  (def cursor (spawn-lsp))
  (request cursor 127 "initialize"
           {:rootUri root-uri :capabilities {:textDocument {:diagnostic {}}}})
  (notify cursor "initialized")
  (def trust (read-output cursor))
  (respond cursor (get-in trust [:id]) {:title "Keep Restricted"})
  (ev/sleep 0.2)
  (def symbols (request cursor 128 "workspace/symbol" {:query "without-progress"}))
  (test (get-in symbols [:result 0 :name]) "indexed-without-progress")
  (exit-lsp cursor)
  (remove-tree root))

(deftest "reuse valid workspace caches without a startup scan"
  (def root (temp-directory "janet-lsp-index-cache-restart"))
  (spit (path/join root "main.janet") "(def cached-across-restarts 1)\n")
  (def root-uri (uri/path->file-uri root))
  (def cache-path (index-cache/path-for root-uri))
  (when (os/stat cache-path) (os/rm cache-path))
  (index-cache/write cache-path root-uri index/default-exclusions
                     (index/scan root index/default-exclusions))

  (def cursor (spawn-lsp))
  (request cursor 143 "initialize"
           {:rootUri root-uri
            :capabilities {:window {:workDoneProgress true}
                           :textDocument {:diagnostic {}}}})
  (notify cursor "initialized")
  # A warm cache emits only the trust request, not progress creation.
  (def trust (read-output cursor))
  (test (get-in trust [:method]) "window/showMessageRequest")
  (respond cursor (get-in trust [:id]) {:title "Keep Restricted"})
  (def symbols (request cursor 144 "workspace/symbol" {:query "across-restarts"}))
  (test (get-in symbols [:result 0 :name]) "cached-across-restarts")
  (spit (path/join root "main.janet") "(def updated-by-watcher 2)\n")
  (notify cursor "workspace/didChangeWatchedFiles"
          {:changes [{:uri (uri/path->file-uri (path/join root "main.janet"))
                      :type 2}]})
  (def updated (request cursor 145 "workspace/symbol" {:query "updated-by-watcher"}))
  (test (get-in updated [:result 0 :name]) "updated-by-watcher")
  (exit-lsp cursor)

  (def restarted (spawn-lsp))
  (request restarted 146 "initialize"
           {:rootUri root-uri
            :capabilities {:window {:workDoneProgress true}
                           :textDocument {:diagnostic {}}}})
  (notify restarted "initialized")
  (def restarted-trust (read-output restarted))
  (test (get-in restarted-trust [:method]) "window/showMessageRequest")
  (respond restarted (get-in restarted-trust [:id]) {:title "Keep Restricted"})
  (def persisted
    (request restarted 147 "workspace/symbol" {:query "updated-by-watcher"}))
  (test (get-in persisted [:result 0 :name]) "updated-by-watcher")
  (exit-lsp restarted)
  (when (os/stat cache-path) (os/rm cache-path))
  (remove-tree root))

(deftest "cancel workspace index subprocesses"
  (def root (temp-directory "janet-lsp-index-cancel"))
  (spit (path/join root "large.janet") (string/repeat "(def indexed-value 1)\n" 20000))
  (def root-uri (uri/path->file-uri root))
  (def cursor (spawn-lsp))
  (request cursor 102 "initialize"
           {:rootUri root-uri
            :capabilities {:window {:workDoneProgress true}}
            :initializationOptions {:trustedWorkspaces [root-uri]}})
  (notify cursor "initialized")
  (def create (read-output cursor))
  (respond cursor (get-in create [:id]) :null)
  (def begin (read-output cursor))
  (def token (get-in begin [:params :token]))
  (notify cursor "window/workDoneProgress/cancel" {:token token})
  (test (get-in (read-output cursor) [:params :value :message]) "Index cancelled")
  (test (get-in (request cursor 103 "janet/serverInfo") [:id]) 103)
  (exit-lsp cursor)
  (remove-tree root))

(deftest "workspace indexer failures finish registered progress"
  (def root (temp-directory "janet-lsp-index-failure"))
  (def root-uri (uri/path->file-uri root))
  (def cursor (spawn-lsp))
  (request cursor 129 "initialize"
           {:rootUri root-uri
            :capabilities {:window {:workDoneProgress true}
                           :textDocument {:diagnostic {}}}})
  (remove-tree root)
  (notify cursor "initialized")
  (def create (read-output cursor))
  (def trust (read-output cursor))
  (respond cursor (get-in create [:id]) :null)
  (test (get-in (read-output cursor) [:params :value :kind]) "begin")
  (respond cursor (get-in trust [:id]) {:title "Keep Restricted"})
  (ev/sleep 0.2)
  (write-output cursor {:jsonrpc "2.0" :id 130 :method "janet/serverInfo" :params {}})
  (def reported (read-output cursor))
  (test (get-in reported [:method]) "window/logMessage")
  (test (get-in reported [:params :type]) 1)
  (test (number? (string/find root-uri (get-in reported [:params :message]))) true)
  (test (string/find "source" (get-in reported [:params :message])) nil)
  (def ended (read-output cursor))
  (test (get-in ended [:params :value :kind]) "end")
  (test (get-in ended [:params :value :message]) "Index failed")
  (test (get-in (read-output cursor) [:id]) 130)
  (exit-lsp cursor))

(deftest: with-process "convert default UTF-16 feature positions" [cursor]
  (def unicode-text "(do \"😀\" string)\n")
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet"
                          :version 1 :text unicode-text}})
  (read-output cursor)
  (def hover
    (request cursor 47 "textDocument/hover"
             {:textDocument {:uri document-uri}
              :position {:line 0 :character 9}}))
  (test (get-in hover [:result :range :start :character]) 9)
  (test (get-in hover [:result :range :end :character]) 15)
  (test (get-in
          (request cursor 48 "textDocument/hover"
                   {:textDocument {:uri document-uri}
                    :position {:line 0 :character 6}})
          [:result])
        :null))

(deftest "negotiate UTF-8 feature positions"
  (def cursor (start-lsp {:general {:positionEncodings ["utf-8" "utf-16"]}}))
  (test (get-in (cursor :initialize) [:result :capabilities :positionEncoding]) "utf-8")
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet"
                          :version 1 :text "(do \"😀\" string)\n"}})
  (read-output cursor)
  (def hover
    (request cursor 49 "textDocument/hover"
             {:textDocument {:uri document-uri}
              :position {:line 0 :character 11}}))
  (test (get-in hover [:result :range :start :character]) 11)
  (test (get-in hover [:result :range :end :character]) 17)
  (exit-lsp cursor))

(deftest "smoke test real client capability profiles"
  (each [client capabilities encoding]
        [["VS Code"
          {:general {:positionEncodings ["utf-16"]}
           :window {:workDoneProgress true}
           :workspace {:workspaceFolders true
                       :workspaceEdit {:documentChanges true
                                       :resourceOperations ["create" "rename" "delete"]}}
           :textDocument {:diagnostic {}
                          :completion {:completionItem {:snippetSupport true}}}}
          "utf-16"]
         ["Neovim"
          {:general {:positionEncodings ["utf-8" "utf-16"]}
           :workspace {:workspaceEdit {:documentChanges true}}
           :textDocument {:diagnostic {} :foldingRange {:lineFoldingOnly true}}}
          "utf-8"]
         ["Helix"
          {:workspace {:workspaceEdit {:documentChanges true}}
           :textDocument {:diagnostic {} :documentLink {:tooltipSupport true}}}
          "utf-16"]
         ["Zed"
          {:general {:positionEncodings ["utf-8"]}
           :workspace {:workspaceEdit {:documentChanges true}}
           :textDocument {:diagnostic {}
                          :completion {:completionItem {:snippetSupport true}}}}
          "utf-8"]]
    (def cursor (spawn-lsp))
    (def initialized
      (request cursor 0 "initialize"
               {:rootUri :null :capabilities capabilities
                :initializationOptions {}}))
    (test (= (get-in initialized
                     [:result :capabilities :positionEncoding])
             encoding)
          true)
    (notify cursor "initialized")
    (def encoded-uri
      (string/replace "format-file-after.txt" "%66ormat-file-after.txt" document-uri))
    (open-text-document cursor encoded-uri "(def value 1)\nvalue\n" 3)
    (def renamed
      (get-in
        (request cursor 317 "textDocument/rename"
                 {:textDocument {:uri encoded-uri}
                  :position {:line 1 :character 2}
                  :newName "renamed"})
        [:result :documentChanges 0]))
    (test (= encoded-uri (get-in renamed [:textDocument :uri])) true)
    (test (get-in renamed [:textDocument :version]) 3)
    (test (all |(= "renamed" ($ :newText)) (renamed :edits)) true)
    (change-text-document cursor encoded-uri
                          "(def renamed 1)\nrenamed\n" 4)
    (test (length
            (get-in
              (request cursor 318 "textDocument/references"
                       {:textDocument {:uri encoded-uri}
                        :position {:line 1 :character 2}
                        :context {:includeDeclaration true}})
              [:result]))
          2)
    (close-text-document cursor encoded-uri)
    (test (get-in
            (request cursor 319 "textDocument/hover"
                     {:textDocument {:uri encoded-uri}
                      :position {:line 0 :character 1}})
            [:result])
          :null)
    (exit-lsp cursor)))

(deftest "reject requests before initialization"
  (def cursor (spawn-lsp))
  (def response (request cursor 30 "janet/serverInfo"))
  (test (get-in response [:error :code]) -32002)
  (test (get-in (request cursor 31 "initialize" {:capabilities {}}) [:id]) 31)
  (request cursor 32 "shutdown")
  (notify cursor "exit")
  (test (wait-process (cursor :process)) 0))

(deftest "exit without shutdown is immediate and unsuccessful"
  (def cursor (spawn-lsp))
  (notify cursor "exit")
  (test (wait-process (cursor :process)) 1))

(deftest "workspace startup requires explicit client trust"
  (def workspace (temp-directory "janet-lsp-trust"))
  (def config-dir (path/join workspace ".janet-lsp"))
  (def startup (path/join config-dir "startup.janet"))
  (def marker (path/join workspace "startup-ran"))
  (def root-uri (string "file://" workspace))
  (os/mkdir config-dir)
  (spit startup (string "(spit " (string/format "%q" marker) " \"ran\")\n{}\n"))

  (def untrusted (spawn-lsp))
  (request untrusted 50 "initialize"
           {:rootUri root-uri :capabilities {} :initializationOptions {}})
  (notify untrusted "initialized")
  (def restricted-prompt (read-output untrusted))
  (test (get-in restricted-prompt [:method]) "window/showMessageRequest")
  (test (get-in restricted-prompt [:params :actions 0 :title]) "Trust for This Session")
  (respond untrusted (get-in restricted-prompt [:id]) {:title "Keep Restricted"})
  (request untrusted 51 "janet/serverInfo")
  (test (os/stat marker) nil)
  (request untrusted 52 "shutdown")
  (notify untrusted "exit")
  (wait-process (untrusted :process))

  (def trusted (spawn-lsp))
  (request trusted 53 "initialize"
           {:rootUri root-uri :capabilities {} :initializationOptions {}})
  (notify trusted "initialized")
  (def trust-prompt (read-output trusted))
  (respond trusted (get-in trust-prompt [:id]) {:title "Trust for This Session"})
  (request trusted 54 "janet/serverInfo")
  (test (os/stat marker :mode) :file)
  (request trusted 55 "shutdown")
  (notify trusted "exit")
  (wait-process (trusted :process))

  (remove-tree workspace))

(deftest "scope analysis across workspace folder changes"
  (def base (temp-directory "janet-lsp-workspaces"))
  (def root-a (path/join base "a"))
  (def root-b (path/join base "b"))
  (def root-c (path/join base "c"))
  (each root [root-a root-b root-c]
    (os/mkdir root))
  (each root [root-a root-b root-c]
    (os/mkdir (path/join root ".janet-lsp")))
  (spit (path/join root-a ".janet-lsp" "startup.janet") "(def workspace-a 1)\n")
  (spit (path/join root-b ".janet-lsp" "startup.janet") "(def workspace-b 1)\n")
  (def marker (path/join root-c "startup-ran"))
  (spit (path/join root-c ".janet-lsp" "startup.janet")
        (string "(spit " (string/format "%q" marker) " \"ran\")\n"
                "(def workspace-c 1)\n"))
  (def file-a (path/join root-a "main.janet"))
  (def file-b (path/join root-b "main.janet"))
  (spit file-a "(workspace-a)\n")
  (spit file-b "(workspace-b)\n")
  (def uri-a (uri/path->file-uri root-a))
  (def uri-b (uri/path->file-uri root-b))
  (def uri-c (uri/path->file-uri root-c))
  (def file-uri-a (uri/path->file-uri file-a))
  (def file-uri-b (uri/path->file-uri file-b))

  (def cursor (spawn-lsp))
  (def initialize
    (request cursor 60 "initialize"
             {:rootUri nil
              :workspaceFolders [{:uri uri-a :name "a"}
                                 {:uri uri-b :name "b"}]
              :capabilities {:textDocument {:diagnostic {}}}
              :initializationOptions {:trustedWorkspaces [uri-a uri-b]}}))
  (test (get-in initialize [:result :capabilities :workspace :workspaceFolders :supported])
        true)
  (notify cursor "initialized")
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri file-uri-a :languageId "janet"
                          :version 1 :text "(workspace-a)\n"}})
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri file-uri-b :languageId "janet"
                          :version 1 :text "(workspace-b)\n"}})

  (def labels-a (completion-labels cursor 61 file-uri-a 0 5))
  (def labels-b (completion-labels cursor 62 file-uri-b 0 5))
  (test (has-value? labels-a "workspace-a") true)
  (test (has-value? labels-a "workspace-b") false)
  (test (has-value? labels-b "workspace-b") true)
  (test (has-value? labels-b "workspace-a") false)

  (def standalone-uri (uri/path->file-uri (path/join base "standalone.janet")))
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri standalone-uri :languageId "janet"
                          :version 1 :text "string\n"}})
  (def standalone-labels (completion-labels cursor 63 standalone-uri 0 3))
  (test (has-value? standalone-labels "string") true)
  (test (has-value? standalone-labels "workspace-a") false)
  (test (has-value? standalone-labels "workspace-b") false)

  (notify cursor "workspace/didChangeWorkspaceFolders"
          {:event {:added [] :removed [{:uri uri-a :name "a"}]}})
  (test (has-value? (completion-labels cursor 64 file-uri-a 0 5) "workspace-a") false)
  (notify cursor "workspace/didChangeWorkspaceFolders"
          {:event {:added [{:uri uri-a :name "a"}] :removed []}})
  (test (has-value? (completion-labels cursor 65 file-uri-a 0 5) "workspace-a") true)

  (notify cursor "workspace/didChangeWorkspaceFolders"
          {:event {:added [{:uri uri-c :name "c"}] :removed []}})
  (def prompt (read-output cursor))
  (test (string/has-prefix? "janet-lsp/workspaceTrust/" (get-in prompt [:id])) true)
  (respond cursor (get-in prompt [:id]) {:title "Keep Restricted"})
  (request cursor 66 "janet/serverInfo")
  (test (os/stat marker) nil)

  (exit-lsp cursor)
  (remove-tree base))

(deftest: with-process "reject duplicate initialize and initialized requests" [cursor]
  (test (get-in (request cursor 33 "initialize" {:capabilities {}}) [:error :code])
        -32600)
  (test (get-in (request cursor 34 "initialized") [:error :code]) -32600)
  (notify cursor "initialized")
  (def prompt (read-output cursor))
  (test (string/has-prefix? "janet-lsp/workspaceTrust/" (get-in prompt [:id])) true)
  (respond cursor (get-in prompt [:id]) {:title "Keep Restricted"})
  (def response (request cursor 35 "janet/serverInfo"))
  (test (get-in response [:error :code]) nil)
  (test (get-in response [:id]) 35))

(deftest "reject requests after shutdown"
  (def cursor (start-lsp))
  (test (get-in (request cursor 36 "shutdown") [:result]) :null)
  (test (get-in (request cursor 37 "janet/serverInfo") [:error :code]) -32600)
  (notify cursor "initialized")
  (notify cursor "exit")
  (test (wait-process (cursor :process)) 0))

(deftest: with-process "reads chunked and consecutive frames" [cursor]
  (write-chunked cursor {:jsonrpc "2.0" :id 10 :method "janet/serverInfo" :params {}})
  (test (get-in (read-output cursor) [:id]) 10)

  (write-output cursor
                {:jsonrpc "2.0" :id 11 :method "janet/serverInfo" :params {}}
                {:jsonrpc "2.0" :id 12 :method "janet/serverInfo" :params {}})
  (test (get-in (read-output cursor) [:id]) 11)
  (test (get-in (read-output cursor) [:id]) 12))

(deftest: with-process "returns method-not-found only for requests" [cursor]
  (def error-response (request cursor 20 "unknown/request"))
  (test (get-in error-response [:id]) 20)
  (test (get-in error-response [:error :code]) -32601)

  (notify cursor "unknown/notification")
  (notify cursor "janet/serverInfo")
  (def next-response (request cursor 21 "janet/serverInfo"))
  (test (get-in next-response [:id]) 21))

(deftest: with-process "recovers from malformed JSON" [cursor]
  (write-body cursor "{\"jsonrpc\":\"2.0\", trailing")
  (def parse-response (read-output cursor))
  (test (get-in parse-response [:id]) :null)
  (test (get-in parse-response [:error :code]) -32700)
  (test (get-in (request cursor 22 "janet/serverInfo") [:id]) 22))

(deftest: with-process "validates request IDs and params" [cursor]
  (write-output cursor {:jsonrpc "2.0" :id true :method "janet/serverInfo" :params {}})
  (def invalid-id (read-output cursor))
  (test (get-in invalid-id [:id]) :null)
  (test (get-in invalid-id [:error :code]) -32600)

  (write-output cursor {:jsonrpc "2.0" :id 23 :method "janet/serverInfo" :params 1})
  (def invalid-params (read-output cursor))
  (test (get-in invalid-params [:id]) 23)
  (test (get-in invalid-params [:error :code]) -32602)

  (write-output cursor {:jsonrpc "1.0" :method "janet/serverInfo"})
  (def invalid-envelope (read-output cursor))
  (test (get-in invalid-envelope [:id]) :null)
  (test (get-in invalid-envelope [:error :code]) -32600))

(deftest: with-process "preserves null and exponent IDs" [cursor]
  (write-output cursor {:jsonrpc "2.0" :id :null :method "janet/serverInfo" :params {}})
  (test (get-in (read-output cursor) [:id]) :null)

  (write-body cursor "{\"jsonrpc\":\"2.0\",\"id\":1e3,\"method\":\"janet/serverInfo\",\"params\":{}}")
  (test (get-in (read-output cursor) [:id]) 1000))

(deftest: with-process "unknown-document requests return null" [cursor]
  (def params {:textDocument {:uri document-uri}
               :position {:line 0 :character 0}})
  (def response (request cursor 24 "textDocument/hover" params))
  (test (get-in response [:id]) 24)
  (test (get-in response [:result]) :null)

  (notify cursor "textDocument/hover" params)
  (test (get-in (request cursor 25 "janet/serverInfo") [:id]) 25))

(deftest: with-process-open "document open publishes diagnostics" [cursor]
  (test (= (get-in (cursor :open) [:params :uri]) document-uri) true)
  (test (get-in (cursor :open) [:params :diagnostics]) @[]))

(deftest "preserve encoded client document URIs"
  (def cursor (start-trusted-lsp))
  (def encoded-uri
    (string/replace "format-file-after.txt" "%66ormat-file-after.txt" document-uri))
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri encoded-uri :languageId "janet"
                          :version 1 :text document-text}})
  (test (= (get-in (read-output cursor) [:params :uri]) encoded-uri) true)
  (def definition
    (request cursor 38 "textDocument/definition"
             {:textDocument {:uri encoded-uri}
              :position {:line 0 :character 6}}))
  (test (= (get-in definition [:result :uri]) encoded-uri) true)
  (notify cursor "textDocument/didClose" {:textDocument {:uri encoded-uri}})
  (test (= (get-in (read-output cursor) [:params :uri]) encoded-uri) true)
  (exit-lsp cursor))

(deftest: with-process "non-file documents avoid filesystem operations" [cursor]
  (def untitled-uri "untitled:janet-lsp-buffer")
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri untitled-uri :languageId "janet"
                          :version 1 :text "(def local-value 1)\nlocal-value\n"}})
  (test (= (get-in (read-output cursor) [:params :uri]) untitled-uri) true)
  (def definition
    (request cursor 39 "textDocument/definition"
             {:textDocument {:uri untitled-uri}
              :position {:line 1 :character 4}}))
  (test (= (get-in definition [:result :uri]) untitled-uri) true)
  (test (get-in definition [:result :range :start :line]) 0))

(deftest: with-process-open "document change publishes diagnostics" [cursor]
  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 2}
           :contentChanges [{:text document-text}]})
  (def response (read-output cursor))
  (test (= (get-in response [:params :uri]) document-uri) true)
  (test (get-in response [:params :version]) 2)
  (test (get-in response [:params :diagnostics]) @[]))

(deftest "negotiate and apply incremental document synchronization"
  (def cursor (start-lsp {:textDocument {:diagnostic {}}}))
  (test (get-in (cursor :initialize)
                [:result :capabilities :textDocumentSync :change])
        2)
  (open-text-document cursor document-uri
                      "(def greeting \"😀\")\ngreeting\n")
  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 2}
           :contentChanges
           [{:range {:start {:line 0 :character 5}
                     :end {:line 0 :character 13}}
             :rangeLength 8
             :text "message"}
            {:range {:start {:line 1 :character 0}
                     :end {:line 1 :character 8}}
             :rangeLength 8
             :text "message"}]})
  (def definition
    (request cursor 139 "textDocument/definition"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 3}}))
  (test (get-in definition [:result :range :start :character]) 0)

  # UTF-16 range positions count the astral symbol as two code units.
  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 3}
           :contentChanges
           [{:range {:start {:line 0 :character 14}
                     :end {:line 0 :character 16}}
             :rangeLength 2
             :text "ok"}]})

  # A failed second event must roll back the valid first event in the batch.
  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 4}
           :contentChanges
           [{:range {:start {:line 0 :character 5}
                     :end {:line 0 :character 12}}
             :text "broken"}
            {:range {:start {:line 8 :character 0}
                     :end {:line 8 :character 1}}
             :text "invalid"}]})
  (def completion
    (request cursor 140 "textDocument/completion"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 7}}))
  (test (has-value? (map |($ :label) (get-in completion [:result :items]))
                    "message")
        true)
  (test (has-value? (map |($ :label) (get-in completion [:result :items]))
                    "broken")
        false)
  (test (get-in completion [:result :items 0 :data :version]) 3)

  # Higher-version ranged edits remain unsafe until a full replacement arrives.
  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 5}
           :contentChanges
           [{:range {:start {:line 0 :character 14}
                     :end {:line 0 :character 16}}
             :text "unsafe"}]})
  (def updated
    (request cursor 141 "textDocument/completion"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 7}}))
  (test (get-in updated [:result :items 0 :data :version]) 3)
  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 6}
           :contentChanges
           [{:range {:start {:line 9 :character 0}
                     :end {:line 9 :character 1}}
             :text "ignored-before-full-replacement"}
            {:text "(def restored \"ok\")\nrestored\n"}]})
  (def resynchronized
    (request cursor 142 "textDocument/completion"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 8}}))
  (test (has-value? (map |($ :label) (get-in resynchronized [:result :items]))
                    "restored")
        true)
  (test (get-in resynchronized [:result :items 0 :data :version]) 6)
  (exit-lsp cursor))

(deftest "pull clients use changed document analysis"
  (def cursor (start-trusted-lsp {:textDocument {:diagnostic {}}}))
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri
                          :languageId "janet"
                          :version 1
                          :text document-text}})
  (def changed-text
    "(def changed (string \"hello\"))\n(string changed)\nchanged\n")
  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 2}
           :contentChanges [{:text changed-text}]})

  (def completion
    (request cursor 40 "textDocument/completion"
             {:textDocument {:uri document-uri}
              :position {:line 2 :character 4}}))
  (def labels (map |($ :label) (get-in completion [:result :items])))
  (test (has-value? labels "changed") true)
  (test (has-value? labels "greeting") false)

  (def hover
    (request cursor 41 "textDocument/hover"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 3}}))
  (test (get-in hover [:result :contents :kind]) "markdown")

  (def signature
    (request cursor 42 "textDocument/signatureHelp"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 8}}))
  (test (string? (get-in signature [:result :signatures 0 :label])) true)

  (def definition
    (request cursor 43 "textDocument/definition"
             {:textDocument {:uri document-uri}
              :position {:line 2 :character 3}}))
  (test (string/has-suffix? "test/resources/format-file-after.txt"
                            (get-in definition [:result :uri]))
        true)

  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 1}
           :contentChanges [{:text "(def stale true)\nstale\n"}]})
  (def after-stale
    (request cursor 44 "textDocument/completion"
             {:textDocument {:uri document-uri}
              :position {:line 2 :character 4}}))
  (def after-stale-labels (map |($ :label) (get-in after-stale [:result :items])))
  (test (has-value? after-stale-labels "changed") true)
  (test (has-value? after-stale-labels "stale") false)
  (exit-lsp cursor))

(deftest "open change and close multiple documents"
  (def cursor (start-lsp))
  (def second-uri (string "file://" (path/abspath "test/resources/format-file-before.txt")))
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet"
                          :version 1 :text document-text}})
  (read-output cursor)
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri second-uri :languageId "janet"
                          :version 4
                          :text "(def closed-only-symbol-xyz 1)\nclosed-only-symbol-xyz\n"}})
  (read-output cursor)
  (notify cursor "textDocument/didChange"
          {:textDocument {:uri second-uri :version 5}
           :contentChanges
           [{:text "(def closed-only-symbol-xyz 2)\nclosed-only-symbol-xyz\n"}]})
  (test (get-in (read-output cursor) [:params :version]) 5)

  (notify cursor "textDocument/didClose" {:textDocument {:uri second-uri}})
  (def cleared (read-output cursor))
  (test (= (get-in cleared [:params :uri]) second-uri) true)
  (test (get-in cleared [:params :diagnostics]) @[])
  (def closed-symbols
    (request cursor 148 "workspace/symbol" {:query "closed-only-symbol-xyz"}))
  (test (get-in closed-symbols [:result]) @[])
  (test (get-in
          (request cursor 45 "textDocument/hover"
                   {:textDocument {:uri second-uri}
                    :position {:line 1 :character 2}})
          [:result])
        :null)

  (def first-completion
    (request cursor 46 "textDocument/completion"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 4}}))
  (test (has-value? (map |($ :label) (get-in first-completion [:result :items]))
                    "greeting")
        true)
  (exit-lsp cursor))

(deftest: with-process-open "hover returns documentation" [cursor]
  (def response
    (request cursor 2 "textDocument/hover"
             {:textDocument {:uri document-uri}
              :position {:line 0 :character 17}}))
  (test (get-in response [:result :contents :kind]) "markdown")
  (test (string/has-prefix? "cfunction" (get-in response [:result :contents :value])) true))

(deftest "hover and definition ignore comments and strings"
  (def cursor (start-lsp {:textDocument {:diagnostic {}}}))
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet" :version 1
                          :text "# string\n\"string\"\nstring\n"}})
  (eachp [id position] [{:line 0 :character 3} {:line 1 :character 3}]
    (test (get-in
            (request cursor (+ 81 id) "textDocument/hover"
                     {:textDocument {:uri document-uri} :position position})
            [:result])
          :null)
    (test (get-in
            (request cursor (+ 83 id) "textDocument/definition"
                     {:textDocument {:uri document-uri} :position position})
            [:result])
          :null))
  (test (get-in
          (request cursor 85 "textDocument/hover"
                   {:textDocument {:uri document-uri}
                    :position {:line 2 :character 3}})
          [:result :contents :kind])
        "markdown")
  (exit-lsp cursor))

(deftest "resolve lexical and indexed definitions"
  (def cursor (start-lsp {:textDocument {:diagnostic {}}}))
  (def second-uri (uri/path->file-uri
                    (path/abspath "test/resources/format-file-before.txt")))
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet" :version 1
                          :text "(def shared 1)\nshared\n"}})
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri second-uri :languageId "janet" :version 1
                          :text "(import ./format-file-after)\nformat-file-after/shared\n"}})
  (def local
    (request cursor 104 "textDocument/definition"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 3}}))
  (test (= (get-in local [:result :uri]) document-uri) true)
  (test (get-in local [:result :range :start :line]) 0)
  (def imported
    (request cursor 105 "textDocument/definition"
             {:textDocument {:uri second-uri}
              :position {:line 1 :character 18}}))
  (test (= (get-in imported [:result :uri]) document-uri) true)

  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 2}
           :contentChanges [{:text "(defn run [value] (let [value 2] value))\n"}]})
  (def shadowed
    (request cursor 106 "textDocument/definition"
             {:textDocument {:uri document-uri}
              :position {:line 0 :character 35}}))
  (test (get-in shadowed [:result :range :start :character]) 24)
  (test (get-in
          (request cursor 107 "textDocument/definition"
                   {:textDocument {:uri document-uri}
                    :position {:line 0 :character 1}})
          [:result])
        :null)
  (exit-lsp cursor))

(deftest "navigate explicit type and implementation metadata"
  (def base (temp-directory "janet-lsp-type-navigation"))
  (def root-a (path/join base "a"))
  (def root-b (path/join base "b"))
  (os/mkdir root-a)
  (os/mkdir root-b)
  (def types-source "(def 😀Shape {})\n(def Drawable {})\n")
  (def main-source
    (string "(import ./types :as types)\n"
            "(def 😀circle {:janet-lsp/type-definition \"types/😀Shape\"} {})\n"
            "(defn draw-circle {:janet-lsp/implements \"types/😀Shape\"} [value] value)\n"
            "(defn draw-both {:janet-lsp/implements [\"types/😀Shape\" \"types/Drawable\"]} [value] value)\n"
            "(def ordinary {})\n"
            "😀circle\n"))
  (def other-source
    (string "(def 😀Shape {})\n"
            "(defn other {:janet-lsp/implements \"😀Shape\"} [value] value)\n"))
  (spit (path/join root-a "types.janet") types-source)
  (spit (path/join root-a "main.janet") main-source)
  (spit (path/join root-b "main.janet") other-source)
  (def root-uri-a (uri/path->file-uri root-a))
  (def root-uri-b (uri/path->file-uri root-b))
  (def types-uri (uri/path->file-uri (path/join root-a "types.janet")))
  (def main-uri (uri/path->file-uri (path/join root-a "main.janet")))
  (def other-uri (uri/path->file-uri (path/join root-b "main.janet")))
  (each [root root-uri] [[root-a root-uri-a] [root-b root-uri-b]]
    (index-cache/write (index-cache/path-for root-uri) root-uri
                       index/default-exclusions
                       (index/scan root index/default-exclusions)))
  (def cursor (spawn-lsp))
  (def initialized
    (request cursor 247 "initialize"
             {:rootUri nil
              :workspaceFolders [{:uri root-uri-a :name "a"}
                                 {:uri root-uri-b :name "b"}]
              :capabilities {:textDocument {:diagnostic {}}}}))
  (test (get-in initialized [:result :capabilities :typeDefinitionProvider]) true)
  (test (get-in initialized [:result :capabilities :implementationProvider]) true)
  (open-text-document cursor types-uri types-source)
  (open-text-document cursor main-uri main-source)
  (open-text-document cursor other-uri other-source)

  (def type-definition
    (request cursor 248 "textDocument/typeDefinition"
             {:textDocument {:uri main-uri}
              :position {:line 5 :character 0}}))
  (test (= (get-in type-definition [:result :uri]) types-uri) true)
  (test (get-in type-definition [:result :range :start :line]) 0)
  (test (get-in type-definition [:result :range :end :character]) 16)

  (def implementations
    (get-in
      (request cursor 249 "textDocument/implementation"
               {:textDocument {:uri types-uri}
                :position {:line 0 :character 7}})
      [:result]))
  (test (and (deep= (map |($ :uri) implementations) @[main-uri main-uri])
             (deep= (map |(get-in $ [:range :start :line]) implementations) @[2 3]))
        true)
  (test (has-value? (map |($ :uri) implementations) other-uri) false)

  (test (get-in
          (request cursor 250 "textDocument/typeDefinition"
                   {:textDocument {:uri main-uri}
                    :position {:line 4 :character 7}})
          [:result])
        :null)
  (test (get-in
          (request cursor 251 "textDocument/implementation"
                   {:textDocument {:uri main-uri}
                    :position {:line 4 :character 7}})
          [:result])
        @[])
  (exit-lsp cursor)
  (remove-tree base))

(deftest "navigate and rename through resolved module aliases"
  (def cursor (start-lsp {:textDocument {:diagnostic {}}}))
  (def imported-uri
    (uri/path->file-uri (path/abspath "test/resources/format-file-before.txt")))
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet" :version 1
                          :text "(def shared 1)\nshared\n"}})
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri imported-uri :languageId "janet" :version 1
                          :text (string "(import ./format-file-after :as module)\n"
                                        "module/shared\n")}})
  (def definition
    (request cursor 136 "textDocument/definition"
             {:textDocument {:uri imported-uri}
              :position {:line 1 :character 8}}))
  (test (= (get-in definition [:result :uri]) document-uri) true)
  (def references
    (request cursor 137 "textDocument/references"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 2}
              :context {:includeDeclaration false}}))
  (test (length (get-in references [:result])) 2)
  (test (has-value? (map |($ :uri) (get-in references [:result])) imported-uri)
        true)

  (def renamed
    (request cursor 138 "textDocument/rename"
             {:textDocument {:uri imported-uri}
              :position {:line 1 :character 8}
              :newName "renamed"}))
  (def imported-change
    (first (filter |(= imported-uri (get-in $ [:textDocument :uri]))
                   (get-in renamed [:result :documentChanges]))))
  (test (get-in imported-change [:edits 0 :newText]) "module/renamed")
  (test (length (get-in renamed [:result :documentChanges])) 2)
  (exit-lsp cursor))

(deftest "return document and workspace symbols"
  (def cursor (start-lsp {:textDocument {:diagnostic {}}}))
  (def second-uri (uri/path->file-uri
                    (path/abspath "test/resources/format-file-before.txt")))
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet" :version 1
                          :text (string "(defn shared [x y] "
                                        "(defn nested [z] z) (+ x y))\n"
                                        "(def value 1)\n")}})
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri second-uri :languageId "janet" :version 1
                          :text "(defn shared [other] other)\n"}})
  (def document-symbols
    (request cursor 108 "textDocument/documentSymbol"
             {:textDocument {:uri document-uri}}))
  (test (get-in document-symbols [:result 0 :name]) "shared")
  (test (get-in document-symbols [:result 0 :children 0 :name]) "x")
  (test (get-in document-symbols [:result 0 :children 1 :name]) "y")
  (test (get-in document-symbols [:result 0 :children 2 :name]) "nested")
  (test (get-in document-symbols [:result 0 :children 2 :children 0 :name]) "z")
  (test (get-in document-symbols [:result 1 :name]) "value")
  (def workspace-symbols (request cursor 109 "workspace/symbol" {:query "sha"}))
  (test (length (get-in workspace-symbols [:result])) 2)
  (test (not= (get-in workspace-symbols [:result 0 :location :uri])
              (get-in workspace-symbols [:result 1 :location :uri]))
        true)
  (exit-lsp cursor))

(deftest "find references without conflating shadows"
  (def cursor (start-lsp {:textDocument {:diagnostic {}}}))
  (def source "(def value 1)\nvalue\n(let [value 2] value)\nvalue\n")
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet" :version 1 :text source}})
  (def outer
    (request cursor 110 "textDocument/references"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 2}
              :context {:includeDeclaration false}}))
  (test (map |(get-in $ [:range :start :line]) (get-in outer [:result])) @[1 3])
  (def inner
    (request cursor 111 "textDocument/references"
             {:textDocument {:uri document-uri}
              :position {:line 2 :character 18}
              :context {:includeDeclaration true}}))
  (test (map |(get-in $ [:range :start :line]) (get-in inner [:result])) @[2 2])

  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 2
                          }
           :contentChanges [{:text "(def value 1)\nvalue\n"}]})
  (def updated
    (request cursor 112 "textDocument/references"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 2}
              :context {:includeDeclaration true}}))
  (test (length (get-in updated [:result])) 2)
  (exit-lsp cursor))

(deftest "prepare and traverse binding-aware call hierarchies"
  (def base (temp-directory "janet-lsp-call-hierarchy"))
  (def root-a (path/join base "a"))
  (def root-b (path/join base "b"))
  (os/mkdir root-a)
  (os/mkdir root-b)
  (def lib-source
    (string "(defn target [x] (target x))\n"
            "(defmacro expand [x] x)\n"))
  (def main-source
    (string "(import ./lib :as lib)\n"
            "(defn caller [x]\n"
            "  (lib/target x)\n"
            "  (lib/target x)\n"
            "  (lib/expand x)\n"
            "  (let [local (fn [y] (lib/target y))]\n"
            "    (local x))\n"
            "  '(lib/target x)\n"
            "  (missing x))\n"
            "(defn same [] nil)\n"
            "(defn same [] nil)\n"
            "(defn ambiguous [] (same))\n"))
  (def other-source
    "(defn target [x] x)\n(defn other [x] (target x))\n")
  (spit (path/join root-a "lib.janet") lib-source)
  (spit (path/join root-a "main.janet") main-source)
  (spit (path/join root-b "main.janet") other-source)
  (def uri-a (uri/path->file-uri root-a))
  (def uri-b (uri/path->file-uri root-b))
  (def lib-uri (uri/path->file-uri (path/join root-a "lib.janet")))
  (def main-uri (uri/path->file-uri (path/join root-a "main.janet")))
  (def other-uri (uri/path->file-uri (path/join root-b "main.janet")))
  (each [root root-uri] [[root-a uri-a] [root-b uri-b]]
    (index-cache/write (index-cache/path-for root-uri) root-uri
                       index/default-exclusions
                       (index/scan root index/default-exclusions)))
  (def cursor (spawn-lsp))
  (def initialized
    (request cursor 237 "initialize"
             {:rootUri nil
              :workspaceFolders [{:uri uri-a :name "a"}
                                 {:uri uri-b :name "b"}]
              :capabilities {:textDocument {:diagnostic {}}}}))
  (test (get-in initialized [:result :capabilities :callHierarchyProvider]) true)
  (open-text-document cursor lib-uri lib-source)
  (open-text-document cursor main-uri main-source)
  (open-text-document cursor other-uri other-source)

  (def prepared-target
    (get-in
      (request cursor 238 "textDocument/prepareCallHierarchy"
               {:textDocument {:uri main-uri}
                :position {:line 2 :character 8}})
      [:result 0]))
  (test (get prepared-target :name) "target")
  (test (= (get prepared-target :uri) lib-uri) true)

  (def incoming
    (get-in (request cursor 239 "callHierarchy/incomingCalls"
                     {:item prepared-target})
            [:result]))
  (test (map |[(get-in $ [:from :name]) (length ($ :fromRanges))] incoming)
        @[["caller" 2] ["local" 1] ["target" 1]])
  (test (all |(or (= lib-uri (get-in $ [:from :uri]))
                  (= main-uri (get-in $ [:from :uri])))
             incoming)
        true)
  (test (has-value? (map |(get-in $ [:from :uri]) incoming) other-uri) false)

  (def prepared-caller
    (get-in
      (request cursor 240 "textDocument/prepareCallHierarchy"
               {:textDocument {:uri main-uri}
                :position {:line 1 :character 8}})
      [:result 0]))
  (def outgoing
    (get-in (request cursor 241 "callHierarchy/outgoingCalls"
                     {:item prepared-caller})
            [:result]))
  (test (map |[(get-in $ [:to :name]) (length ($ :toRanges))] outgoing)
        @[["expand" 1] ["local" 1] ["target" 2]])

  (def prepared-local
    (get-in
      (request cursor 242 "textDocument/prepareCallHierarchy"
               {:textDocument {:uri main-uri}
                :position {:line 6 :character 7}})
      [:result 0]))
  (test (get prepared-local :name) "local")
  (test (get-in prepared-local [:range :start :character]) 8)
  (test (get-in prepared-local [:selectionRange :start :character]) 8)
  (test (> (get-in prepared-local [:range :end :character])
           (get-in prepared-local [:selectionRange :end :character]))
        true)
  (def local-outgoing
    (get-in (request cursor 243 "callHierarchy/outgoingCalls"
                     {:item prepared-local})
            [:result]))
  (test (map |(get-in $ [:to :name]) local-outgoing) @["target"])

  (test (get-in
          (request cursor 244 "textDocument/prepareCallHierarchy"
                   {:textDocument {:uri main-uri}
                    :position {:line 8 :character 4}})
          [:result])
        :null)
  (def ambiguous
    (get-in
      (request cursor 245 "textDocument/prepareCallHierarchy"
               {:textDocument {:uri main-uri}
                :position {:line 11 :character 22}})
      [:result]))
  (test ambiguous :null)

  (notify cursor "$/cancelRequest" {:id 246})
  (def cancelled
    (request cursor 246 "callHierarchy/incomingCalls" {:item prepared-target}))
  (test (get-in cancelled [:error :code]) -32800)
  (exit-lsp cursor)
  (remove-tree base))

(deftest "highlight bindings and return structural ranges"
  (def cursor
    (start-lsp {:textDocument
                {:diagnostic {}
                 :foldingRange {:lineFoldingOnly true :rangeLimit 20}}}))
  (def source
    (string "# first 😀 comment\n"
            "# second comment\n"
            "(def value 1)\n"
            "value\n"
            "(let [value 2]\n"
            "  (do \"😀\"\n"
            "    value))\n"
            "value\n"))
  (open-text-document cursor document-uri source)

  (def inner
    (request cursor 216 "textDocument/documentHighlight"
             {:textDocument {:uri document-uri}
              :position {:line 6 :character 7}}))
  (test (map |(get-in $ [:range :start :line]) (get-in inner [:result]))
        @[4 6])
  (test (map |($ :kind) (get-in inner [:result])) @[3 2])
  (def inner-from-declaration
    (request cursor 225 "textDocument/documentHighlight"
             {:textDocument {:uri document-uri}
              :position {:line 4 :character 8}}))
  (test (map |(get-in $ [:range :start :line])
             (get-in inner-from-declaration [:result]))
        @[4 6])
  (def outer
    (request cursor 217 "textDocument/documentHighlight"
             {:textDocument {:uri document-uri}
              :position {:line 3 :character 2}}))
  (test (map |(get-in $ [:range :start :line]) (get-in outer [:result]))
        @[2 3 7])
  (test (map |($ :kind) (get-in outer [:result])) @[3 2 2])
  (test (get-in
          (request cursor 218 "textDocument/documentHighlight"
                   {:textDocument {:uri document-uri}
                    :position {:line 0 :character 4}})
          [:result])
        @[])

  (def folding
    (request cursor 219 "textDocument/foldingRange"
             {:textDocument {:uri document-uri}}))
  (test (map |[($ :startLine) ($ :endLine) (get $ :kind)]
             (get-in folding [:result]))
        @[[0 1 "comment"] [4 6 nil] [5 6 nil]])
  (test (get-in folding [:result 0 :startCharacter]) nil)

  (def selection
    (request cursor 220 "textDocument/selectionRange"
             {:textDocument {:uri document-uri}
              :positions [{:line 6 :character 7}
                          {:line 5 :character 7}]}))
  (test (get-in selection [:result 0 :range :start])
        @{:line 6 :character 4})
  (test (get-in selection [:result 0 :range :end])
        @{:line 6 :character 9})
  (test (get-in selection [:result 0 :parent :range :start])
        @{:line 5 :character 2})
  # The astral character occupies two UTF-16 units.
  (test (get-in selection [:result 1 :range :start])
        @{:line 5 :character 6})
  (test (get-in selection [:result 1 :range :end])
        @{:line 5 :character 10})

  (change-text-document cursor document-uri "(\n  [value\n" 2)
  (def malformed-folding
    (request cursor 221 "textDocument/foldingRange"
             {:textDocument {:uri document-uri}}))
  (test (map |[($ :startLine) ($ :endLine)] (get-in malformed-folding [:result]))
        @[[0 2] [1 2]])
  (def malformed-selection
    (request cursor 222 "textDocument/selectionRange"
             {:textDocument {:uri document-uri}
              :positions [{:line 1 :character 5}]}))
  (test (get-in malformed-selection [:result 0 :range :start])
        @{:line 1 :character 3})
  (test (get-in malformed-selection [:result 0 :range :end])
        @{:line 1 :character 8})
  (test (get-in malformed-selection [:result 0 :parent :range :start :line]) 1)

  (change-text-document cursor document-uri "'\n(do\n  value)\n" 3)
  (def reader-folding
    (request cursor 228 "textDocument/foldingRange"
             {:textDocument {:uri document-uri}}))
  (test (map |[($ :startLine) ($ :endLine)] (get-in reader-folding [:result]))
        @[[0 2] [1 2]])
  (def reader-selection
    (request cursor 229 "textDocument/selectionRange"
             {:textDocument {:uri document-uri}
              :positions [{:line 2 :character 4}]}))
  (test (get-in reader-selection [:result 0 :parent :range :start :line]) 1)
  (test (get-in reader-selection [:result 0 :parent :parent :range :start :line]) 0)

  (change-text-document cursor document-uri
                        "(def value 1)\nvalue\n(let [value 2\n" 4)
  (test (get-in
          (request cursor 230 "textDocument/documentHighlight"
                   {:textDocument {:uri document-uri}
                    :position {:line 2 :character 8}})
          [:result])
        @[])
  (test (get-in
          (request cursor 232 "textDocument/definition"
                   {:textDocument {:uri document-uri}
                    :position {:line 2 :character 8}})
          [:result])
        :null)
  (test (get-in
          (request cursor 236 "textDocument/references"
                   {:textDocument {:uri document-uri}
                    :position {:line 2 :character 8}
                    :context {:includeDeclaration true}})
          [:result])
        @[])

  (change-text-document cursor document-uri "(let [x 1] x x)\n" 5)
  (def later-local
    (request cursor 234 "textDocument/documentHighlight"
             {:textDocument {:uri document-uri}
              :position {:line 0 :character 13}}))
  (test (map |(get-in $ [:range :start :character])
             (get-in later-local [:result]))
        @[6 11 13])
  (def later-definition
    (request cursor 235 "textDocument/definition"
             {:textDocument {:uri document-uri}
              :position {:line 0 :character 13}}))
  (test (get-in later-definition [:result :range :start :character]) 6)
  (exit-lsp cursor))

(deftest "link module and source paths in the owning workspace"
  (def base (temp-directory "janet-lsp-document-links"))
  (def root-a (path/join base "a"))
  (def root-b (path/join base "b"))
  (os/mkdir root-a)
  (os/mkdir root-b)
  (each root [root-a root-b]
    (spit (path/join root "target.janet")
          (string "(def root-name " (string/format "%q" root) ")\n")))
  (spit (path/join root-a "script.janet") "(def script true)\n")
  (spit (path/join root-a "bare.janet") "(def bare true)\n")
  (spit (path/join root-a "extensionless") "not a Janet module\n")
  (spit (path/join root-a "extensionless.janet") "(def extensionless true)\n")
  (spit (path/join root-a "module-path.janet") "(def dynamic true)\n")
  (spit (path/join root-a "source-path") "(def dynamic-source true)\n")
  (spit (path/join root-a "source.txt") "(def source true)\n")
  (def main-a (path/join root-a "main.janet"))
  (def source
    (string "(import ./target :as target)\n"
            "(require \"./target\")\n"
            "(dofile \"./script.janet\")\n"
            "(dofile \"./source.txt\")\n"
            "(use ./target ./script)\n"
            "(require \"./extensionless\")\n"
            "(dofile \"./bare\")\n"
            "# (require \"./missing\")\n"
            "'(require \"./target\")\n"
            "(quote (require \"./target\"))\n"
            "|(require \"./target\")\n"
            "@(dofile \"./source.txt\")\n"
            "(require module-path)\n"
            "(dofile source-path)\n"
            "~(do ,(require \"./target\"))\n"
            "@[(require \"./target\")]\n"
            "~~(,(require \"./target\"))\n"
            "(require \"./unterminated)\n"))
  (spit main-a source)
  (def uri-a (uri/path->file-uri root-a))
  (def uri-b (uri/path->file-uri root-b))
  (def main-uri (uri/path->file-uri main-a))
  (each [root root-uri] [[root-a uri-a] [root-b uri-b]]
    (index-cache/write (index-cache/path-for root-uri) root-uri
                       index/default-exclusions
                       (index/scan root index/default-exclusions)))
  (def cursor (spawn-lsp))
  (request cursor 223 "initialize"
           {:rootUri nil
            :workspaceFolders [{:uri uri-a :name "a"}
                               {:uri uri-b :name "b"}]
            :capabilities
            {:textDocument {:diagnostic {}
                            :documentLink {:tooltipSupport true}}}})
  (open-text-document cursor main-uri source)
  (def response
    (request cursor 224 "textDocument/documentLink"
             {:textDocument {:uri main-uri}}))
  (def links (get-in response [:result]))
  (test (length links) 10)
  (test (map |(get-in $ [:range :start :line]) links)
        @[0 1 2 3 4 4 5 10 14 15])
  (test (get-in links [0 :range :start]) @{:line 0 :character 8})
  (test (get-in links [0 :range :end]) @{:line 0 :character 16})
  (test (get-in links [1 :range :start]) @{:line 1 :character 10})
  (test (get-in links [1 :range :end]) @{:line 1 :character 18})
  (test (= (get-in links [0 :target])
           (uri/path->file-uri (path/join root-a "target.janet")))
        true)
  (test (not= (get-in links [0 :target])
              (uri/path->file-uri (path/join root-b "target.janet")))
        true)
  (test (= (get-in links [2 :target])
           (uri/path->file-uri (path/join root-a "script.janet")))
        true)
  (test (= (get-in links [3 :target])
           (uri/path->file-uri (path/join root-a "source.txt")))
        true)
  (test (= (get-in links [6 :target])
           (uri/path->file-uri (path/join root-a "extensionless.janet")))
        true)
  (test (get-in links [0 :tooltip]) "Open Janet source")
  (def import-definition
    (request cursor 225 "textDocument/definition"
             {:textDocument {:uri main-uri}
              :position {:line 0 :character 12}}))
  (test (= (get-in import-definition [:result :uri])
           (uri/path->file-uri (path/join root-a "target.janet")))
        true)
  (test (get-in import-definition [:result :range :start])
        @{:line 0 :character 0})

  (def nested (path/join root-a "nested"))
  (os/mkdir nested)
  (spit (path/join root-a "runtime.janet") "(def runtime :root)\n")
  (spit (path/join nested "runtime.janet") "(def runtime :nested)\n")
  (def nested-uri (uri/path->file-uri (path/join nested "main.janet")))
  (open-text-document cursor nested-uri "(dofile \"./runtime.janet\")\n")
  (def nested-links
    (get-in (request cursor 231 "textDocument/documentLink"
                     {:textDocument {:uri nested-uri}})
            [:result]))
  (test (= (get-in nested-links [0 :target])
           (uri/path->file-uri (path/join root-a "runtime.janet")))
        true)
  (exit-lsp cursor)
  (remove-tree base))

(deftest "encode structural ranges as negotiated UTF-8 positions"
  (def cursor
    (start-lsp {:general {:positionEncodings ["utf-8"]}
                :textDocument {:diagnostic {}}}))
  (def source "\"😀\" (do\n  value)\n")
  (open-text-document cursor document-uri source)
  (def folding
    (request cursor 226 "textDocument/foldingRange"
             {:textDocument {:uri document-uri}}))
  (test (get-in folding [:result 0 :startCharacter]) 7)
  (def selection
    (request cursor 227 "textDocument/selectionRange"
             {:textDocument {:uri document-uri}
              :positions [{:line 0 :character 1}]}))
  (test (get-in selection [:result 0 :range :start :character]) 0)
  (test (get-in selection [:result 0 :range :end :character]) 6)
  (change-text-document cursor document-uri "" 2)
  (def empty-selection
    (request cursor 233 "textDocument/selectionRange"
             {:textDocument {:uri document-uri}
              :positions [{:line 0 :character 0}]}))
  (test (get-in empty-selection [:result 0 :range :start])
        @{:line 0 :character 0})
  (test (get-in empty-selection [:result 0 :range :end])
        @{:line 0 :character 0})
  (exit-lsp cursor))

(deftest "prepare and apply versioned workspace renames"
  (def cursor (start-lsp {:textDocument {:diagnostic {}}}))
  (def source "(def value 1)\nvalue\n(let [value 2] value)\nvalue\n")
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet" :version 7 :text source}})
  (def prepared
    (request cursor 113 "textDocument/prepareRename"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 2}}))
  (test (get-in prepared [:result :placeholder]) "value")
  (test (get-in prepared [:result :range :start :character]) 0)
  (test (get-in prepared [:result :range :end :character]) 5)
  (def renamed
    (request cursor 114 "textDocument/rename"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 2}
              :newName "renamed"}))
  (test (get-in renamed [:result :documentChanges 0 :textDocument :version]) 7)
  (test (map |(get-in $ [:range :start :line])
             (get-in renamed [:result :documentChanges 0 :edits]))
        @[0 1 3])
  (test (all |(= "renamed" ($ :newText))
             (get-in renamed [:result :documentChanges 0 :edits]))
        true)
  (test (get-in
          (request cursor 115 "textDocument/rename"
                   {:textDocument {:uri document-uri}
                    :position {:line 1 :character 2}
                    :newName "two symbols"})
          [:error :code])
        -32602)
  (test (get-in
          (request cursor 116 "textDocument/prepareRename"
                   {:textDocument {:uri document-uri}
                    :position {:line 2 :character 1}})
          [:error :code])
        -32602)
  (exit-lsp cursor))

(deftest "rename import aliases selective exports and module files safely"
  (def root (temp-directory "janet-lsp-module-rename"))
  (def module-path (path/join root "module.janet"))
  (def reexport-path (path/join root "reexport.janet"))
  (def consumer-path (path/join root "consumer.janet"))
  (def closed-path (path/join root "closed.janet"))
  (def prefix-path (path/join root "prefix.janet"))
  (def default-path (path/join root "default.janet"))
  (def a-root (path/join root "a"))
  (def b-root (path/join root "b"))
  (os/mkdir a-root)
  (os/mkdir b-root)
  (def a-foo-path (path/join a-root "foo.janet"))
  (def b-foo-path (path/join b-root "foo.janet"))
  (def ambiguous-path (path/join root "ambiguous.janet"))
  (spit module-path "(def shared 1)\n(def- collision 2)\n")
  (spit reexport-path
        "(import ./module :only [shared] :prefix \"nested/\" :export true)\n")
  (spit consumer-path
        (string "(import ./reexport :as api :only [nested/shared])\n"
                "api/nested/shared api/nested/shared\n"))
  (spit closed-path "(import ./module :as closed)\nclosed/shared\n")
  (spit prefix-path "(import ./module :prefix \"old-\")\nold-shared\n")
  (spit default-path "(import ./module)\nmodule/shared\n")
  (spit a-foo-path "(def a 1)\n")
  (spit b-foo-path "(def b 2)\n")
  (spit ambiguous-path
        (string "(import ./a/foo :only [a])\n"
                "(import ./b/foo :only [b])\nfoo/a foo/b\n"))
  (def root-uri (uri/path->file-uri root))
  (def module-uri (uri/path->file-uri module-path))
  (def reexport-uri (uri/path->file-uri reexport-path))
  (def consumer-uri (uri/path->file-uri consumer-path))
  (def closed-uri (uri/path->file-uri closed-path))
  (def prefix-uri (uri/path->file-uri prefix-path))
  (def default-uri (uri/path->file-uri default-path))
  (def ambiguous-uri (uri/path->file-uri ambiguous-path))
  (index-cache/write (index-cache/path-for root-uri) root-uri
                     index/default-exclusions
                     (index/scan root index/default-exclusions))
  (def cursor (spawn-lsp))
  (request cursor 281 "initialize"
           {:rootUri root-uri
            :capabilities
            {:textDocument {:diagnostic {}}
             :workspace {:workspaceEdit {:documentChanges true
                                          :resourceOperations ["rename"]}}}})
  (open-text-document cursor module-uri
                      "(def shared 1)\n(def- collision 2)\n")
  (open-text-document
    cursor reexport-uri
    "(import ./module :only [shared] :prefix \"nested/\" :export true)\n")
  (open-text-document
    cursor consumer-uri
    (string "(import ./reexport :as api :only [nested/shared])\n"
            "api/nested/shared api/nested/shared\n"))
  (open-text-document cursor prefix-uri
                      "(import ./module :prefix \"old-\")\nold-shared\n")
  (open-text-document
    cursor ambiguous-uri
    (string "(import ./a/foo :only [a])\n"
            "(import ./b/foo :only [b])\nfoo/a foo/b\n"))

  (def alias-prepared
    (request cursor 282 "textDocument/prepareRename"
             {:textDocument {:uri consumer-uri}
              :position {:line 0 :character 24}}))
  (test (get-in alias-prepared [:result :placeholder]) "api")
  (def alias-use-prepared
    (request cursor 289 "textDocument/prepareRename"
             {:textDocument {:uri consumer-uri}
              :position {:line 1 :character 1}}))
  (test (get-in alias-use-prepared [:result :range :end :character]) 3)
  (test (get-in
          (request cursor 294 "textDocument/prepareRename"
                   {:textDocument {:uri consumer-uri}
                    :position {:line 1 :character 19}})
          [:result :range :start :character])
        18)
  (def member-prepared
    (request cursor 290 "textDocument/prepareRename"
             {:textDocument {:uri consumer-uri}
              :position {:line 1 :character 13}}))
  (test (get-in member-prepared [:result :placeholder]) "shared")
  (test (get-in member-prepared [:result :range :start :character]) 11)
  (test (get-in
          (request cursor 291 "textDocument/prepareRename"
                   {:textDocument {:uri consumer-uri}
                    :position {:line 1 :character 3}})
          [:error :code])
        -32602)
  (def alias-renamed
    (get-in
      (request cursor 283 "textDocument/rename"
               {:textDocument {:uri consumer-uri}
                :position {:line 0 :character 24}
                :newName "client"})
      [:result :documentChanges 0]))
  (test (get-in alias-renamed [:textDocument :version]) 1)
  (test (map |($ :newText) (alias-renamed :edits))
        @["client" "client/nested/shared" "client/nested/shared"])

  (def prefix-renamed
    (get-in
      (request cursor 288 "textDocument/rename"
               {:textDocument {:uri prefix-uri}
                :position {:line 0 :character 30}
                :newName "new-"})
      [:result :documentChanges 0]))
  (test (map |($ :newText) (prefix-renamed :edits)) @["new-" "new-shared"])

  (def member-renamed
    (get-in
      (request cursor 284 "textDocument/rename"
               {:textDocument {:uri reexport-uri}
                :position {:line 0 :character 30}
                :newName "renamed"})
      [:result :documentChanges]))
  (test (length member-renamed) 6)
  (test (all |(string/has-suffix? "renamed" ($ :newText))
             (catseq [change :in member-renamed edit :in (change :edits)] edit))
        true)
  (test (get-in
          (request cursor 296 "textDocument/rename"
                   {:textDocument {:uri module-uri}
                    :position {:line 0 :character 7}
                    :newName "collision"})
          [:error :code])
        -32602)

  (def file-renamed
    (get-in
      (request cursor 285 "textDocument/rename"
               {:textDocument {:uri reexport-uri}
                :position {:line 0 :character 10}
                :newName "renamed-module"})
      [:result :documentChanges]))
  (def reexport-change
    (first (filter |(= reexport-uri (get-in $ [:textDocument :uri])) file-renamed)))
  (def closed-change
    (first (filter |(= closed-uri (get-in $ [:textDocument :uri])) file-renamed)))
  (def rename-operation (first (filter |(= "rename" ($ :kind)) file-renamed)))
  (def default-change
    (first (filter |(= default-uri (get-in $ [:textDocument :uri])) file-renamed)))
  (test (get-in reexport-change [:edits 0 :newText]) "./renamed-module")
  (test (get-in reexport-change [:textDocument :version]) 1)
  (test (get-in closed-change [:textDocument :version]) nil)
  (test (get-in closed-change [:edits 0 :newText]) "./renamed-module")
  (test (map |($ :newText) (default-change :edits))
        @["./renamed-module" "renamed-module/shared"])
  (test (= (rename-operation :oldUri) module-uri) true)
  (test (= (rename-operation :newUri)
           (uri/path->file-uri (path/join root "renamed-module.janet")))
        true)
  (def one-module-renamed
    (get-in
      (request cursor 295 "textDocument/rename"
               {:textDocument {:uri ambiguous-uri}
                :position {:line 0 :character 13}
                :newName "renamed"})
      [:result :documentChanges]))
  (def ambiguous-change
    (first (filter |(= ambiguous-uri (get-in $ [:textDocument :uri]))
                   one-module-renamed)))
  (test (map |($ :newText) (ambiguous-change :edits))
        @["./a/renamed" "renamed/a"])
  (test (get-in
          (request cursor 292 "textDocument/rename"
                   {:textDocument {:uri reexport-uri}
                    :position {:line 0 :character 10}
                    :newName "closed"})
          [:error :code])
        -32602)
  (change-text-document
    cursor prefix-uri
    "(import ./module :as chosen :prefix \"ignored-\")\nchosen/shared\n" 2)
  (test (get-in
          (request cursor 293 "textDocument/prepareRename"
                   {:textDocument {:uri prefix-uri}
                    :position {:line 0 :character 43}})
          [:error :code])
        -32602)
  (exit-lsp cursor)

  (def unsupported (spawn-lsp))
  (request unsupported 286 "initialize"
           {:rootUri root-uri :capabilities {:textDocument {:diagnostic {}}}})
  (open-text-document
    unsupported reexport-uri
    "(import ./module :only [shared] :prefix \"nested/\" :export true)\n")
  (test (get-in
          (request unsupported 287 "textDocument/prepareRename"
                   {:textDocument {:uri reexport-uri}
                    :position {:line 0 :character 10}})
          [:error :code])
        -32602)
  (exit-lsp unsupported)
  (remove-tree root))

(deftest "track active signature parameters across encodings"
  (each encoding ["utf-16" "utf-8"]
    (def capabilities
      (if (= encoding "utf-8")
        {:general {:positionEncodings ["utf-8"]}
         :textDocument {:diagnostic {}}}
        {:textDocument {:diagnostic {}}}))
    (def cursor (start-lsp capabilities))
    (def source "(string \"😀\"\n  \"b\")")
    (notify cursor "textDocument/didOpen"
            {:textDocument {:uri document-uri :languageId "janet"
                            :version 1 :text source}})
    (def response
      (request cursor 91 "textDocument/signatureHelp"
               {:textDocument {:uri document-uri}
                :position {:line 1 :character 5}}))
    (test (get-in response [:result :activeSignature]) 0)
    (test (get-in response [:result :activeParameter]) 1)
    (test (get-in response [:result :signatures 0 :parameters 0 :label]) "&")
    (test (get-in response [:result :signatures 0 :parameters 1 :label]) "xs")
    (test (string? (get-in response [:result :signatures 0 :documentation :value])) true)
    (test (get-in
            (request cursor 92 "textDocument/signatureHelp"
                     {:textDocument {:uri document-uri}
                      :position {:line 0 :character 0}})
            [:result])
          :null)
    (exit-lsp cursor)))

(deftest "emit sorted semantic tokens in negotiated encodings"
  (each encoding ["utf-16" "utf-8"]
    (def capabilities
      (if (= encoding "utf-8")
        {:general {:positionEncodings ["utf-8"]}
         :textDocument {:diagnostic {}}}
        {:textDocument {:diagnostic {}}}))
    (def cursor (start-lsp capabilities))
    (test (get-in (cursor :initialize)
                   [:result :capabilities :semanticTokensProvider :full])
          @{:delta true})
    (test (get-in (cursor :initialize)
                  [:result :capabilities :semanticTokensProvider :range])
          true)
    (notify cursor "textDocument/didOpen"
            {:textDocument {:uri document-uri :languageId "janet" :version 1
                            :text "(def 😀 1)\n😀 # hidden\n\"hidden\"\n"}})
    (def response
      (request cursor 117 "textDocument/semanticTokens/full"
               {:textDocument {:uri document-uri}}))
    (def data (get-in response [:result :data]))
    (def result-id (get-in response [:result :resultId]))
    (test (string? result-id) true)
    (test (= 0 (% (length data) 5)) true)
    (test (all |(>= $ 0)
               (map |(data $) (range 0 (length data) 5)))
          true)
    (test (has-value? (map |(data (+ $ 2)) (range 0 (length data) 5))
                       (if (= encoding "utf-8") 4 2))
          true)
    (def unchanged
      (request cursor 297 "textDocument/semanticTokens/full/delta"
               {:textDocument {:uri document-uri}
                :previousResultId result-id}))
    (test (get-in unchanged [:result :edits]) @[])
    (test (= (get-in unchanged [:result :resultId]) result-id) true)
    (def ranged-response
      (request cursor 298 "textDocument/semanticTokens/range"
               {:textDocument {:uri document-uri}
                :range {:start {:line 1 :character 0}
                        :end {:line 2 :character 0}}}))
    (def ranged (get-in ranged-response [:result :data]))
    (test (ranged 0) 1)
    (test (all |(= 0 (ranged $)) (range 5 (length ranged) 5)) true)
    (def overlapping
      (get-in
        (request cursor 301 "textDocument/semanticTokens/range"
                 {:textDocument {:uri document-uri}
                  :range {:start {:line 1 :character 1}
                          :end {:line 1 :character 2}}})
        [:result :data]))
    (test (overlapping 0) 1)
    (test (overlapping 1) 0)

    (change-text-document cursor document-uri
                          "(def 😀 2)\n😀 # hidden\n\"hidden\"\n" 2)
    (def changed
      (get-in
        (request cursor 299 "textDocument/semanticTokens/full/delta"
                 {:textDocument {:uri document-uri}
                  :previousResultId result-id})
        [:result]))
    (test (string? (changed :resultId)) true)
    # Token positions and classifications are unchanged when only a literal changes.
    (test (changed :edits) @[])
    (exit-lsp cursor)))

(deftest "classify semantic definitions bindings and syntax"
  (def cursor (start-lsp {:textDocument {:diagnostic {}}}))
  (def source
    (string "(import ./module :as mod)\n"
            "(defmacro m [] nil)\n"
            "(defn run [arg] (let [local 1] (+ arg local)))\n"
            "(m) \"text\" # comment\n"
            "\"multi\nline\"\n"))
  (open-text-document cursor document-uri source)
  (def classified-response
    (request cursor 300 "textDocument/semanticTokens/full"
             {:textDocument {:uri document-uri}}))
  (def data (get-in classified-response [:result :data]))
  (def decoded @[])
  (var line 0)
  (var character 0)
  (each index (range 0 (length data) 5)
    (def delta-line (data index))
    (set line (+ line delta-line))
    (set character (if (= delta-line 0)
                     (+ character (data (inc index)))
                     (data (inc index))))
    (array/push decoded
                {:line line :character character
                 :length (data (+ index 2)) :type (data (+ index 3))
                 :modifiers (data (+ index 4))}))
  (test (has-value? (map |[($ :line) ($ :type)] decoded) [0 0]) true)
  (test (has-value? (map |[($ :line) ($ :type) ($ :modifiers)] decoded)
                    [1 3 3])
        true)
  (test (length (filter |(= 5 ($ :type)) decoded)) 2)
  (test (length (filter |(and (= 5 ($ :type)) (= 1 ($ :modifiers))) decoded)) 1)
  (test (has-value? (map |[($ :type) ($ :modifiers)] decoded) [4 1]) true)
  (test (has-value? (map |($ :type) decoded) 10) true)
  (test (>= (length (filter |(= 7 ($ :type)) decoded)) 3) true)
  (test (has-value? (map |($ :type) decoded) 9) true)
  (exit-lsp cursor))

(deftest "provide trusted evidence-based code lenses lazily"
  (def root (temp-directory "janet-lsp-code-lenses"))
  (def source-path (path/join root "main.janet"))
  (def source
    (string "(defn used [] nil)\n"
            "(used)\n"
            "(defn main :entry [] nil)\n"
            "(defn check :flycheck [] nil)\n"
            "(deftest \"works\" (test true))\n"))
  (spit source-path source)
  (def root-uri (uri/path->file-uri root))
  (def source-uri (uri/path->file-uri source-path))
  (index-cache/write (index-cache/path-for root-uri) root-uri
                     index/default-exclusions
                     (index/scan root index/default-exclusions))
  (def cursor (spawn-lsp))
  (def initialized
    (request cursor 302 "initialize"
             {:rootUri root-uri
              :initializationOptions {:trustedWorkspaces [root-uri]}
              :capabilities {:textDocument {:diagnostic {}}}}))
  (test (get-in initialized [:result :capabilities :codeLensProvider])
        @{:resolveProvider true})
  (open-text-document cursor source-uri source)
  (def lenses
    (get-in
      (request cursor 303 "textDocument/codeLens"
               {:textDocument {:uri source-uri}})
      [:result]))
  (test (all |(nil? ($ :command)) lenses) true)
  (test (frequencies (map |(get-in $ [:data :kind]) lenses))
        @{"references" 3 "run" 1 "flycheck" 1 "test" 1})
  (def reference-lens
    (first (filter |(and (= "references" (get-in $ [:data :kind]))
                         (= "used" (get-in $ [:data :name]))) lenses)))
  (def resolved-reference
    (get-in (request cursor 304 "codeLens/resolve" reference-lens) [:result]))
  (test (get-in resolved-reference [:command :title]) "1 reference")
  (test (get-in resolved-reference [:command :command])
        "editor.action.showReferences")
  (test (length (get-in resolved-reference [:command :arguments 2])) 1)
  (def test-lens (first (filter |(= "test" (get-in $ [:data :kind])) lenses)))
  (def resolved-test
    (get-in (request cursor 305 "codeLens/resolve" test-lens) [:result]))
  (test (get-in resolved-test [:command :command]) "janet-lsp.runTest")
  (change-text-document cursor source-uri (string source "\n") 2)
  (test (get-in (request cursor 306 "codeLens/resolve" test-lens)
                [:result :command])
        nil)
  (exit-lsp cursor)

  (def restricted (spawn-lsp))
  (request restricted 307 "initialize"
           {:rootUri root-uri
            :initializationOptions
            {:codeLenses {:references false :tests false
                           :flycheck false :runnable false}}
            :capabilities {:textDocument {:diagnostic {}}}})
  (open-text-document restricted source-uri source)
  (test (get-in
          (request restricted 308 "textDocument/codeLens"
                   {:textDocument {:uri source-uri}})
          [:result])
        @[])
  (test (get-in
          (request restricted 309 "workspace/executeCommand"
                   {:command "janet-lsp.runDefinition"
                    :arguments [source-uri "main"]})
          [:error :code])
        -32600)
  (exit-lsp restricted)
  (remove-tree root))

(deftest "emit conservative configurable parameter hints"
  (def source "(do \"😀\" (put table (put source key value) value) (string a b) (unknown x))")
  (def cursor (start-lsp {:textDocument {:diagnostic {}}}))
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet" :version 1 :text source}})
  (def response
    (request cursor 121 "textDocument/inlayHint"
             {:textDocument {:uri document-uri}
              :range {:start {:line 0 :character 0}
                      :end {:line 0 :character 100}}}))
  (test (map |($ :label) (get-in response [:result])) @["ds:" "ds:"])
  (test (all |(and (= 2 ($ :kind)) ($ :paddingRight)
                   (= "markdown" (get-in $ [:tooltip :kind])))
             (get-in response [:result]))
        true)
  (def restricted
    (request cursor 122 "textDocument/inlayHint"
             {:textDocument {:uri document-uri}
              :range {:start {:line 0 :character 24}
                      :end {:line 0 :character 35}}}))
  (test (length (get-in restricted [:result])) 1)
  (exit-lsp cursor)

  (def disabled
    (start-lsp {:textDocument {:diagnostic {}}}
               {:inlayHints {:parameterNames false}}))
  (test (get-in (disabled :initialize) [:result :capabilities :inlayHintProvider]) false)
  (notify disabled "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet" :version 1 :text source}})
  (test (get-in
          (request disabled 123 "textDocument/inlayHint"
                   {:textDocument {:uri document-uri}
                    :range {:start {:line 0 :character 0}
                            :end {:line 0 :character 100}}})
          [:result])
        @[])
  (exit-lsp disabled))

(deftest "emit static constant and return metadata hints"
  (each encoding ["utf-16" "utf-8"]
    (def capabilities
      (if (= encoding "utf-8")
        {:general {:positionEncodings ["utf-8"]}
         :textDocument {:diagnostic {}}}
        {:textDocument {:diagnostic {}}}))
    (def cursor
      (start-lsp capabilities
                 {:inlayHints {:parameterNames true :constantValues true
                               :returnMetadata true}}))
    (def source
      (string "(def 😀 42)\n"
              "(defn run @{:janet-lsp/returns \"number\"} [x &named option]\n"
              "  (+ 😀 x))\n"
              "(run 1 :option 2)\n"
              "😀\n"
              "(defn target [wrong] nil)\n"
              "(defn shadow [target] (target 1))\n"
              "(def bad 1 2)\n"
              "bad\n"))
    (open-text-document cursor document-uri source)
    (def hints
      (get-in
        (request cursor 310 "textDocument/inlayHint"
                 {:textDocument {:uri document-uri}
                  :range {:start {:line 0 :character 0}
                          :end {:line 9 :character 0}}})
        [:result]))
    (test (frequencies (map |($ :label) hints))
          @{" -> number" 1 " = 42" 2 "x:" 1})
    (test (all |(and (= "markdown" (get-in $ [:tooltip :kind]))
                     (nil? ($ :textEdits))) hints)
          true)
    (test (has-value? (map |($ :label) hints) "wrong:") false)
    (test (has-value? (map |($ :label) hints) " = 2") false)
    (def final-constant
      (last (filter |(= " = 42" ($ :label)) hints)))
    (test (= (get-in final-constant [:position :character])
             (if (= encoding "utf-8") 4 2))
          true)
    (def restricted
      (get-in
        (request cursor 311 "textDocument/inlayHint"
                 {:textDocument {:uri document-uri}
                  :range {:start {:line 3 :character 0}
                          :end {:line 4 :character 0}}})
        [:result]))
    (test (map |($ :label) restricted) @["x:"])
    (exit-lsp cursor)))

(deftest: with-process-open "pull diagnostics returns a full report" [cursor]
  (def response
    (request cursor 3 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (get-in response [:result :kind]) "full")
  (test (get-in response [:result :items]) @[])
  (def unchanged
    (request cursor 150 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}
              :previousResultId (get-in response [:result :resultId])}))
  (test (get-in unchanged [:result :kind]) "unchanged")
  (test (deep= (get-in unchanged [:result :resultId])
               (get-in response [:result :resultId]))
        true))

(deftest "change diagnostic severity configuration and source suppressions"
  (def cursor
    (start-lsp {:textDocument {:diagnostic {}}}
               {:diagnostics {:undefinedSymbol "off"}}))
  (open-text-document cursor document-uri
                      "(defn run [unused] missing)\n")
  (def disabled
    (request cursor 151 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (has-value? (map |($ :code) (get-in disabled [:result :items]))
                    "janet.lint.undefined-symbol")
        false)
  (test (has-value? (map |($ :code) (get-in disabled [:result :items]))
                    "janet.lint.unused-parameter")
        true)

  (notify cursor "workspace/didChangeConfiguration"
          {:settings {:janetLsp
                      {:diagnostics {:undefinedSymbol "hint"
                                     :unusedParameter "off"}}}})
  (def changed
    (request cursor 152 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}
              :previousResultId (get-in disabled [:result :resultId])}))
  (test (get-in changed [:result :kind]) "full")
  (def undefined
    (first (filter |(= "janet.lint.undefined-symbol" ($ :code))
                   (get-in changed [:result :items]))))
  (test (undefined :severity) 4)
  (test (get-in undefined [:range :start :character]) 19)
  (test (get-in undefined [:range :end :character]) 26)
  (test (has-value? (map |($ :code) (get-in changed [:result :items]))
                    "janet.lint.unused-parameter")
        false)

  (change-text-document
    cursor document-uri
    "# janet-lsp: ignore-next-line undefinedSymbol\nmissing\n" 2)
  (def suppressed
    (request cursor 153 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (get-in suppressed [:result :items]) @[])
  (exit-lsp cursor))

(deftest "republish push diagnostics after configuration changes"
  (def cursor (start-lsp))
  (open-text-document cursor document-uri "missing-push-name\n")
  (def initial (read-output cursor))
  (test (get-in initial [:params :diagnostics 0 :severity]) 1)
  (notify cursor "workspace/didChangeConfiguration"
          {:settings {:diagnostics {:undefinedSymbol "information"}}})
  (def republished (read-output cursor))
  (test (get-in republished [:method]) "textDocument/publishDiagnostics")
  (test (get-in republished [:params :diagnostics 0 :severity]) 3)
  (exit-lsp cursor))

(deftest "configure and suppress parser diagnostic categories"
  (def cursor
    (start-lsp {:textDocument {:diagnostic {}}}
               {:diagnostics {:parse "off"}}))
  (open-text-document cursor document-uri "(")
  (def disabled
    (request cursor 160 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (get-in disabled [:result :items]) @[])
  (notify cursor "workspace/didChangeConfiguration" {:settings {}})
  (change-text-document
    cursor document-uri "# janet-lsp: disable parse\n(" 2)
  (def suppressed
    (request cursor 161 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (get-in suppressed [:result :items]) @[])
  (exit-lsp cursor))

(deftest "request pull diagnostic refresh after configuration changes"
  (def cursor
    (start-lsp {:textDocument {:diagnostic {}}
                :workspace {:diagnostics {:refreshSupport true}}}))
  (notify cursor "workspace/didChangeConfiguration"
          {:settings {:diagnostics {:shadowing "hint"}}})
  (def refresh (read-output cursor))
  (test (get-in refresh [:method]) "workspace/diagnostic/refresh")
  (test (length (get-in refresh [:params])) 0)
  (respond cursor (get refresh :id) :null)
  (test (get-in (request cursor 157 "janet/serverInfo") [:id]) 157)
  (exit-lsp cursor))

(deftest "provide full and unchanged workspace diagnostic reports"
  (def root (temp-directory "janet-lsp-workspace-diagnostics"))
  (def clean-path (path/join root "clean.janet"))
  (def broken-path (path/join root "broken.janet"))
  (spit clean-path "(def clean 1)\n")
  (spit broken-path "workspace-missing\n")
  (def root-uri (uri/path->file-uri root))
  (def cache-path (index-cache/path-for root-uri))
  (index-cache/write cache-path root-uri index/default-exclusions
                     (index/scan root index/default-exclusions))
  (def cursor (spawn-lsp))
  (request cursor 154 "initialize"
           {:rootUri root-uri
            :capabilities {:textDocument {:diagnostic {}}}})
  (def report (request cursor 155 "workspace/diagnostic" {}))
  (test (length (get-in report [:result :items])) 2)
  (def broken-uri (uri/path->file-uri broken-path))
  (def broken
    (first (filter |(= broken-uri ($ :uri)) (get-in report [:result :items]))))
  (test (broken :kind) "full")
  (test (get-in broken [:items 0 :code]) "janet.lint.undefined-symbol")
  (test (broken :version) :null)
  (def previous
    (map |{:uri ($ :uri) :value ($ :resultId)} (get-in report [:result :items])))
  (def unchanged
    (request cursor 156 "workspace/diagnostic" {:previousResultIds previous}))
  (test (all |(= "unchanged" ($ :kind)) (get-in unchanged [:result :items])) true)
  (os/rm clean-path)
  (notify cursor "workspace/didChangeWatchedFiles"
          {:changes [{:uri (uri/path->file-uri clean-path) :type 3}]})
  (def removed
    (request cursor 162 "workspace/diagnostic" {:previousResultIds previous}))
  (def cleared
    (first (filter |(= (uri/path->file-uri clean-path) ($ :uri))
                   (get-in removed [:result :items]))))
  (test (cleared :kind) "full")
  (test (cleared :version) :null)
  (test (cleared :items) @[])
  (open-text-document cursor broken-uri "(def unsaved-clean 1)\n" 2)
  (def overlaid
    (request cursor 158 "workspace/diagnostic" {:previousResultIds previous}))
  (def open-report
    (first (filter |(= broken-uri ($ :uri)) (get-in overlaid [:result :items]))))
  (test (open-report :kind) "full")
  (test (open-report :version) 2)
  (test (open-report :items) @[])
  (exit-lsp cursor)
  (when (os/stat cache-path) (os/rm cache-path))
  (remove-tree root))

(deftest "invalidate dependent diagnostics after open module changes"
  (def root (temp-directory "janet-lsp-diagnostic-dependencies"))
  (def module-path (path/join root "module.janet"))
  (def main-path (path/join root "main.janet"))
  (def module-source "(def exported 1)\n")
  (def main-source
    "(import ./module :only [exported] :prefix \"\")\nexported\n")
  (spit module-path module-source)
  (spit main-path main-source)
  (def root-uri (uri/path->file-uri root))
  (def module-uri (uri/path->file-uri module-path))
  (def main-uri (uri/path->file-uri main-path))
  (def cache-path (index-cache/path-for root-uri))
  (index-cache/write cache-path root-uri index/default-exclusions
                     (index/scan root index/default-exclusions))
  (def cursor (spawn-lsp))
  (request cursor 163 "initialize"
           {:rootUri root-uri
            :capabilities {:textDocument {:diagnostic {}}}})
  (open-text-document cursor module-uri module-source)
  (open-text-document cursor main-uri main-source)
  (def initial
    (request cursor 164 "textDocument/diagnostic"
             {:textDocument {:uri main-uri}}))
  (test (get-in initial [:result :items]) @[])
  (change-text-document cursor module-uri "(def replacement 1)\n" 2)
  (def changed
    (request cursor 165 "textDocument/diagnostic"
             {:textDocument {:uri main-uri}
              :previousResultId (get-in initial [:result :resultId])}))
  (test (get-in changed [:result :kind]) "full")
  (test (get-in changed [:result :items 0 :code])
        "janet.lint.undefined-symbol")
  (test (get-in changed [:result :items 0 :message]) "undefined symbol exported")
  (exit-lsp cursor)

  (def pushed (spawn-lsp))
  (request pushed 166 "initialize" {:rootUri root-uri :capabilities {}})
  (open-text-document pushed module-uri module-source)
  (read-output pushed)
  (open-text-document pushed main-uri main-source)
  (read-output pushed)
  (change-text-document pushed module-uri "(def replacement 1)\n" 2)
  (def module-published (read-output pushed))
  (def dependent-published (read-output pushed))
  (test (= (get-in module-published [:params :uri]) module-uri) true)
  (test (= (get-in dependent-published [:params :uri]) main-uri) true)
  (test (get-in dependent-published [:params :diagnostics 0 :code])
        "janet.lint.undefined-symbol")
  (exit-lsp pushed)
  (when (os/stat cache-path) (os/rm cache-path))
  (remove-tree root))

(deftest: with-process "push diagnostics report zero-based parse positions" [cursor]
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet"
                          :version 1 :text "("}})
  (def published (read-output cursor))
  (test (get-in published [:params :diagnostics 0 :range :start :line]) 0)
  (test (get-in published [:params :diagnostics 0 :range :start :character]) 0)
  (test (string/has-prefix? "parse error:"
                            (get-in published [:params :diagnostics 0 :message]))
        true))

(deftest: with-process "report safe unused parameters without workspace trust" [cursor]
  (open-text-document cursor document-uri "(defn run [unused] 1)\n")
  (def published (read-output cursor))
  (test (get-in published [:params :diagnostics 0 :code])
        "janet.lint.unused-parameter")
  (test (get-in published [:params :diagnostics 0 :severity]) 2))

(deftest: with-process "offer only current deterministic diagnostic fixes" [cursor]
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet"
                          :version 1 :text "("}})
  (def published (read-output cursor))
  (def diagnostic (get-in published [:params :diagnostics 0]))
  (test (get diagnostic :code) "janet.parse.unclosed-delimiter")
  (def params {:textDocument {:uri document-uri}
               :range {:start {:line 0 :character 0} :end {:line 0 :character 1}}
               :context {:diagnostics [diagnostic] :only ["quickfix"]}})
  (def actions (request cursor 118 "textDocument/codeAction" params))
  (test (get-in actions [:result 0 :kind]) "quickfix")
  (test (get-in actions [:result 0 :edit :documentChanges 0 :textDocument :version]) 1)
  (test (get-in actions [:result 0 :edit :documentChanges 0 :edits 0 :newText]) ")")
  (def filtered
    (request cursor 119 "textDocument/codeAction"
             {:textDocument {:uri document-uri}
              :range (get params :range)
              :context {:diagnostics [diagnostic] :only ["source"]}}))
  (test (get-in filtered [:result]) @[])
  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 2}
           :contentChanges [{:text "("}]})
  (read-output cursor)
  (test (get-in (request cursor 120 "textDocument/codeAction" params) [:result]) @[]))

(deftest "offer deterministic call and unused-binding fixes"
  (def cursor (start-lsp {:textDocument {:diagnostic {}}}))
  (def source
    (string "(defn target [first second &named ok] (do ok (+ first second)))\n"
            "(target 1)\n"
            "(target 1 2 :bad 3)\n"
            "(defn edge [first second &named ok] (do ok first second))\n"
            "(edge :value)\n"
            "(edge :bad 2 :bad 3)\n"
            "(defn run [unused] nil)\n"
            "(let [binding 1] nil)\n"))
  (open-text-document cursor document-uri source)
  (def diagnostics
    (get-in
      (request cursor 264 "textDocument/diagnostic"
               {:textDocument {:uri document-uri}})
      [:result :items]))
  (defn diagnostic [code &opt callee]
    (first (filter |(and (= code ($ :code))
                         (or (nil? callee) (= callee (get-in $ [:data :callee]))))
                   diagnostics)))
  (defn action [id code &opt callee]
    (get-in
      (request cursor id "textDocument/codeAction"
               {:textDocument {:uri document-uri}
                :range ((diagnostic code callee) :range)
                :context {:diagnostics [(diagnostic code callee)]
                          :only ["quickfix"]}})
      [:result 0]))

  (def missing (action 265 "janet.call.missing-arguments"))
  (test (get missing :title) "Insert 1 missing argument")
  (test (get-in missing [:edit :documentChanges 0 :edits 0 :newText]) " nil")
  (def unsupported (action 266 "janet.call.unknown-named-argument"))
  (test (get unsupported :title) "Remove unsupported :bad argument")
  (test (get-in unsupported [:edit :documentChanges 0 :edits 0 :newText]) "")
  (def keyword-positional (action 278 "janet.call.missing-arguments" "edge"))
  (test (get-in keyword-positional
                [:edit :documentChanges 0 :edits 0 :range :start :character])
        12)
  (test (get-in keyword-positional [:edit :documentChanges 0 :edits 0 :newText])
        " nil")
  (def repeated-unknown
    (action 279 "janet.call.unknown-named-argument" "edge"))
  (test (get-in repeated-unknown
                [:edit :documentChanges 0 :edits 0 :range :start :character])
        13)
  (def unused-parameter (action 267 "janet.lint.unused-parameter"))
  (test (get unused-parameter :title) "Mark unused as intentionally unused")
  (test (get-in unused-parameter [:edit :documentChanges 0 :edits 0 :newText]) "_")
  (def unused-binding (action 268 "janet.lint.unused-binding"))
  (test (get unused-binding :title) "Mark binding as intentionally unused")
  (test (get-in unused-binding [:edit :documentChanges 0 :edits 0 :newText]) "_")
  (test (get-in unused-binding [:edit :documentChanges 0 :textDocument :version]) 1)
  (exit-lsp cursor))

(deftest "fix missing and unused imports and sort safe import blocks"
  (def root (temp-directory "janet-lsp-code-action-imports"))
  (def a-path (path/join root "a.janet"))
  (def b-path (path/join root "b.janet"))
  (def c-path (path/join root "c.janet"))
  (def main-path (path/join root "main.janet"))
  (spit a-path "(def alpha 1)\n")
  (spit b-path "(def beta 2)\n")
  (spit c-path "(def missing-value 3)\n")
  (def source
    (string "(import ./b)\n"
            "(import ./a :as a)\n"
            "missing-value\n"))
  (spit main-path source)
  (def root-uri (uri/path->file-uri root))
  (def main-uri (uri/path->file-uri main-path))
  (index-cache/write (index-cache/path-for root-uri) root-uri
                     index/default-exclusions
                     (index/scan root index/default-exclusions))
  (def cursor (spawn-lsp))
  (request cursor 269 "initialize"
           {:rootUri root-uri :capabilities {:textDocument {:diagnostic {}}}})
  (open-text-document cursor main-uri source)
  (def diagnostics
    (get-in
      (request cursor 270 "textDocument/diagnostic"
               {:textDocument {:uri main-uri}})
      [:result :items]))
  (def undefined
    (first (filter |(= "janet.lint.undefined-symbol" ($ :code)) diagnostics)))
  (def missing-action
    (get-in
      (request cursor 271 "textDocument/codeAction"
               {:textDocument {:uri main-uri}
                :range (undefined :range)
                :context {:diagnostics [undefined] :only ["quickfix"]}})
      [:result 0]))
  (test (get missing-action :title) "Import missing-value")
  (test (string/find "(import ./c :only [missing-value] :prefix \"\")"
                     (get-in missing-action
                             [:edit :documentChanges 0 :edits 0 :newText]))
        1)

  (def unused
    (first (filter |(= "janet.lint.unused-import" ($ :code)) diagnostics)))
  (def remove-action
    (get-in
      (request cursor 272 "textDocument/codeAction"
               {:textDocument {:uri main-uri}
                :range (unused :range)
                :context {:diagnostics [unused] :only ["quickfix"]}})
      [:result 0]))
  (test (get remove-action :title) "Mark import as intentionally unused")
  (test (get remove-action :isPreferred) true)
  (test (get-in remove-action [:edit :documentChanges 0 :edits 0 :newText])
        " :as _b")

  (def sort-action
    (get-in
      (request cursor 273 "textDocument/codeAction"
               {:textDocument {:uri main-uri}
                :range {:start {:line 0 :character 0}
                        :end {:line 2 :character 0}}
                :context {:diagnostics @[] :only ["source.sortImports"]}})
      [:result 0]))
  (test (get sort-action :kind) "source.sortImports")
  (test (get-in sort-action [:edit :documentChanges 0 :edits 0 :newText])
        "(import ./a :as a)\n(import ./b)")
  (def organize-action
    (get-in
      (request cursor 274 "textDocument/codeAction"
               {:textDocument {:uri main-uri}
                :range {:start {:line 0 :character 0}
                        :end {:line 2 :character 0}}
                :context {:diagnostics @[] :only ["source.organizeImports"]}})
      [:result 0]))
  (test (get organize-action :kind) "source.organizeImports")
  (test (= (get-in organize-action [:edit :documentChanges 0 :edits 0 :newText])
           (get-in sort-action [:edit :documentChanges 0 :edits 0 :newText]))
        true)
  (def source-actions
    (get-in
      (request cursor 280 "textDocument/codeAction"
               {:textDocument {:uri main-uri}
                :range {:start {:line 0 :character 0}
                        :end {:line 2 :character 0}}
                :context {:diagnostics @[] :only ["source"]}})
      [:result]))
  (test (map |($ :kind) source-actions)
        @["source.sortImports" "source.organizeImports"])

  (change-text-document cursor main-uri
                        "# attached\n(import ./b :as b)\n(import ./a :as a)\n" 2)
  (test (get-in
          (request cursor 275 "textDocument/codeAction"
                   {:textDocument {:uri main-uri}
                    :range {:start {:line 0 :character 0}
                            :end {:line 2 :character 18}}
                    :context {:diagnostics @[] :only ["source.organizeImports"]}})
          [:result])
        @[])

  (change-text-document cursor main-uri
                        "#!/usr/bin/env janet\r\nmissing-value\r\n" 3)
  (def crlf-diagnostics
    (get-in
      (request cursor 276 "textDocument/diagnostic"
               {:textDocument {:uri main-uri}})
      [:result :items]))
  (def crlf-undefined
    (first (filter |(= "janet.lint.undefined-symbol" ($ :code)) crlf-diagnostics)))
  (def crlf-action
    (get-in
      (request cursor 277 "textDocument/codeAction"
               {:textDocument {:uri main-uri}
                :range (crlf-undefined :range)
                :context {:diagnostics [crlf-undefined] :only ["quickfix"]}})
      [:result 0]))
  (test (get-in crlf-action
                [:edit :documentChanges 0 :edits 0 :range :start :line])
        1)
  (test (get-in crlf-action [:edit :documentChanges 0 :edits 0 :newText])
        "(import ./c :only [missing-value] :prefix \"\")\r\n")
  (exit-lsp cursor)
  (remove-tree root))

(deftest "pull diagnostics survive compile and bounded runtime failures"
  (def cursor (start-trusted-lsp {:textDocument {:diagnostic {}}}))
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet"
                          :version 1 :text "(freeze )"}})
  (def compile-report
    (request cursor 67 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (get-in compile-report [:result :items 0 :range :start :line]) 0)
  (test (get-in compile-report [:result :items 0 :range :start :character]) 0)
  (test (string/has-prefix? "compile error:"
                            (get-in compile-report [:result :items 0 :message]))
        true)

  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 2}
           :contentChanges [{:text "(defn boom :flycheck [] (error \"boom\"))\n(boom)\n"}]})
  (def runtime-report
    (request cursor 68 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (string/has-prefix? "runtime error:"
                            (get-in runtime-report [:result :items 0 :message]))
        true)

  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 3}
           :contentChanges
           [{:text (string "(defn skipped [unused] 1)\n"
                           (string/repeat " " 1048577))}]})
  (def bounded-report
    (request cursor 72 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (string/has-prefix? "analysis limit exceeded:"
                            (get-in bounded-report [:result :items 0 :message]))
        true)
  (test (length (get-in bounded-report [:result :items])) 1)
  (test (get-in (request cursor 69 "janet/serverInfo") [:id]) 69)
  (exit-lsp cursor))

(deftest "diagnose stable function argument mistakes"
  (def cursor (start-trusted-lsp {:textDocument {:diagnostic {}}}))
  (open-text-document cursor document-uri
                      "(defn run [required unused &named option] required)\n(run)\n")
  (def missing
    (request cursor 124 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (not (not (any? (map |(string/find "expects at least 2 arguments, got 0"
                                           ($ :message))
                             (get-in missing [:result :items])))))
        true)
  (test (has-value? (map |($ :code) (get-in missing [:result :items]))
                    "janet.lint.unused-parameter")
        true)

  (change-text-document cursor document-uri
                        "(defn run [required &named option] required)\n(run 1 :unknown 2)\n"
                        2)
  (def unknown
    (request cursor 125 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (not (not (any? (map |(string/find "unused named argument :unknown"
                                           ($ :message))
                             (get-in unknown [:result :items])))))
        true)

  (change-text-document cursor document-uri
                        "(defn run [] missing-name)\n"
                        3)
  (def unknown-symbol
    (request cursor 126 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (string/has-prefix? "compile error: unknown symbol missing-name"
                            (get-in unknown-symbol [:result :items 0 :message]))
        true)
  (exit-lsp cursor))

(deftest "provide restricted call diagnostics and named argument help"
  (def cursor (start-lsp {:textDocument {:diagnostic {}}}))
  (def definition
    "(defn run [required &opt optional &named option other] required)")
  (open-text-document cursor document-uri (string definition "\n(run)\n"))
  (def report
    (request cursor 131 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (has-value? (map |($ :code) (get-in report [:result :items]))
                    "janet.call.missing-arguments")
        true)

  (def partial-call "(run 1 nil :op)")
  (change-text-document cursor document-uri
                        (string definition "\n" partial-call "\n") 2)
  (def labels
    (completion-labels cursor 132 document-uri 1 (dec (length partial-call))))
  (test (has-value? labels ":option") true)

  (def keyword-positional "(run 1 :option )")
  (change-text-document cursor document-uri
                        (string definition "\n" keyword-positional "\n") 3)
  (def after-keyword-positional
    (completion-labels cursor 133 document-uri 1
                       (dec (length keyword-positional))))
  (test (has-value? after-keyword-positional ":option") true)

  (def named-call "(run 1 nil :option 2 )")
  (change-text-document cursor document-uri
                        (string definition "\n" named-call "\n") 4)
  (def remaining
    (completion-labels cursor 134 document-uri 1 (dec (length named-call))))
  (test (has-value? remaining ":option") false)
  (test (has-value? remaining ":other") true)
  (def signature
    (request cursor 135 "textDocument/signatureHelp"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character (dec (length named-call))}}))
  (test (get-in signature [:result :signatures 0 :label])
        "(run required &opt optional &named option other)")
  (test (map |($ :label)
             (get-in signature [:result :signatures 0 :parameters]))
        @["required" "optional" ":option" ":other"])
  (test (get-in signature [:result :activeParameter]) 2)
  (exit-lsp cursor))

(deftest: with-process "cancel pull diagnostic requests" [cursor]
  (notify cursor "$/cancelRequest" {:id 70})
  (def cancelled
    (request cursor 70 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (get-in cancelled [:error :code]) -32800)
  (notify cursor "$/cancelRequest" {:id 159})
  (test (get-in (request cursor 159 "workspace/diagnostic" {}) [:error :code]) -32800)
  (each [id method]
        [[312 "workspace/symbol"]
          [313 "textDocument/references"]
          [314 "textDocument/rename"]
         [315 "textDocument/semanticTokens/full"]]
    (notify cursor "$/cancelRequest" {:id id})
    (test (get-in
            (request cursor id method
                     (if (= method "workspace/symbol")
                       {:query "value"}
                       {:textDocument {:uri document-uri}
                        :position {:line 0 :character 0}
                        :newName "renamed"}))
            [:error :code])
          -32800))
  (write-output cursor
                {:jsonrpc "2.0" :id 316 :method "workspace/symbol"
                 :params {:query "value"}}
                {:jsonrpc "2.0" :method "$/cancelRequest" :params {:id 316}})
  (test (get-in (read-output cursor) [:error :code]) -32800)
  (test (get-in (request cursor 71 "janet/serverInfo") [:id]) 71))

(deftest: with-process-open "completion includes core and local bindings" [cursor]
  (def response
    (request cursor 4 "textDocument/completion"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 0}
              :context {:triggerKind 1}}))
  (def labels (map |($ :label) (get-in response [:result :items])))
  (test (get-in response [:result :isIncomplete]) false)
  (test (has-value? labels "string") true)
  (test (has-value? labels "greeting") true))

(deftest: with-process-open "completion items resolve documentation" [cursor]
  (def completion
    (request cursor 5 "textDocument/completion"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 0}
              :context {:triggerKind 1}}))
  (def item (first (filter |(= "string" ($ :label))
                           (get-in completion [:result :items]))))
  (put item :detail "preserved detail")
  (put item :sortText "001")
  (put item :filterText "string")
  (put item :insertText "string")
  (def response (request cursor 6 "completionItem/resolve" item))
  (test (get-in response [:result :label]) "string")
  (test (get-in response [:result :kind]) 3)
  (test (get-in response [:result :detail]) "preserved detail")
  (test (get-in response [:result :sortText]) "001")
  (test (get-in response [:result :filterText]) "string")
  (test (get-in response [:result :insertText]) "string")
  (test (= (get-in response [:result :data :uri]) document-uri) true)
  (test (get-in response [:result :documentation :kind]) "markdown")
  (test (string/has-prefix? "cfunction"
                            (get-in response [:result :documentation :value]))
        true))

(deftest "complete syntax, modules, members, and missing imports by context"
  (def root (temp-directory "janet-lsp-context-completion"))
  (def module-path (path/join root "module.janet"))
  (def main-path (path/join root "main.janet"))
  (spit module-path
        (string "(defn exported [value] value)\n"
                "(def other :custom 1)\n"
                "(def secret :private 2)\n"
                "(def ambiguous 3)\n"
                "(defn- hidden [] nil)\n"))
  (spit (path/join root "other.janet") "(def ambiguous 4)\n")
  (spit (path/join root "second.janet") "(def second-export 5)\n")
  (spit (path/join root "my module.janet") "(def spaced-export 6)\n")
  (spit main-path "")
  (def root-uri (uri/path->file-uri root))
  (def main-uri (uri/path->file-uri main-path))
  (def cache-path (index-cache/path-for root-uri))
  (index-cache/write cache-path root-uri index/default-exclusions
                     (index/scan root index/default-exclusions))
  (def cursor (spawn-lsp))
  (request cursor 167 "initialize"
           {:rootUri root-uri
            :capabilities
            {:textDocument
             {:diagnostic {}
              :completion {:completionItem {:snippetSupport true}}}}})

  (var completion-version 0)
  (defn complete [id source line character]
    (+= completion-version 1)
    (if (get-in cursor [:completion-document-open])
      (change-text-document cursor main-uri source completion-version)
      (do
        (open-text-document cursor main-uri source completion-version)
        (put cursor :completion-document-open true)))
    (get-in (request cursor id "textDocument/completion"
                     {:textDocument {:uri main-uri}
                      :position {:line line :character character}})
            [:result :items]))

  (def import-items (complete 168 "(import ./mo)" 0 12))
  (test (has-value? (map |($ :label) import-items) "./module") true)
  (def require-items (complete 169 "(require \"./mo\")" 0 14))
  (test (has-value? (map |($ :label) require-items) "./module") true)
  (def require-item (first (filter |(= "./module" ($ :label)) require-items)))
  (test (get-in require-item [:textEdit :range :start :character]) 10)
  (test (get-in require-item [:textEdit :range :end :character]) 14)
  (def dofile-items (complete 170 "(dofile \"./mo\")" 0 13))
  (test (has-value? (map |($ :label) dofile-items) "./module.janet") true)
  (def use-items (complete 171 "(use ./mo)" 0 9))
  (test (has-value? (map |($ :label) use-items) "./module") true)
  (def spaced-module-items (complete 210 "(import ./my)" 0 12))
  (def spaced-module
    (first (filter |(= "./my module" ($ :label)) spaced-module-items)))
  (test (get-in spaced-module [:textEdit :newText]) "\"./my module\"")
  (def quoted-spaced-items (complete 211 "(require \"./my\")" 0 14))
  (def quoted-spaced
    (first (filter |(= "./my module" ($ :label)) quoted-spaced-items)))
  (test (get-in quoted-spaced [:textEdit :newText]) "./my module")

  (def member-source "(import ./module :as module)\nmodule/ex\n")
  (def member-items (complete 172 member-source 1 9))
  (test (has-value? (map |($ :label) member-items) "module/exported") true)
  (def member-item
    (first (filter |(= "module/exported" ($ :label)) member-items)))
  (test (get-in member-item [:textEdit :range :start :line]) 1)
  (test (get-in member-item [:textEdit :range :start :character]) 0)
  (test (get-in member-item [:textEdit :range :end :line]) 1)
  (test (get-in member-item [:textEdit :range :end :character]) 9)
  (test (get-in member-item [:textEdit :newText]) "module/exported")
  (test (has-value? (map |($ :label) member-items) "module/hidden") false)
  (test (has-value? (map |($ :label) member-items) "module/secret") false)
  (def alias-items (complete 182 "(import ./module :as module)\nmod\n" 1 3))
  (test (has-value? (map |($ :label) alias-items) "module") false)
  (def used-items
    (complete 183 "(use ./module)\nexp\n" 1 3))
  (def used-item (first (filter |(= "exported" ($ :label)) used-items)))
  (test (string/has-prefix? "Imported from" (used-item :detail)) true)
  (test (get used-item :additionalTextEdits) nil)
  (def multi-use-items
    (complete 199 "(use ./module ./second)\nsecond\n" 1 6))
  (test (has-value? (map |($ :label) multi-use-items) "second-export") true)
  (def prefixed-items
    (complete 200 "(import ./module :prefix \"m-\")\nm-ex\n" 1 4))
  (test (has-value? (map |($ :label) prefixed-items) "m-exported") true)
  (test (has-value? (map |($ :label) prefixed-items) "module/exported") false)
  (def unprefixed-items
    (complete 201 "(import ./module :prefix \"\")\nexp\n" 1 3))
  (test (has-value? (map |($ :label) unprefixed-items) "exported") true)
  (def only-items
    (complete 184 "(import ./module :only [ex" 0 26))
  (test (has-value? (map |($ :label) only-items) "exported") true)

  (def binding-items (complete 173 "(defn run [&" 0 12))
  (test (has-value? (map |($ :label) binding-items) "&opt") true)
  (test (has-value? (map |($ :label) binding-items) "string") false)
  (test (has-value? (map |($ :label) (complete 185 "(def str" 0 8)) "string")
        false)
  (test (has-value? (map |($ :label) (complete 186 "(let [str" 0 9)) "string")
        false)
  (test (has-value? (map |($ :label) (complete 187 "(let [value str" 0 15))
                    "string")
        true)
  (test (has-value? (map |($ :label) (complete 196 "(defn run [] [str" 0 17))
                    "string")
        true)
  (test (has-value? (map |($ :label) (complete 202 "(loop [str" 0 10)) "string")
        false)
  (test (has-value?
          (map |($ :label) (complete 212 "(loop [x :in xs str" 0 19))
          "string")
        false)

  (def table-key-items (complete 174 "{:custom 1}\n{:cu" 1 4))
  (test (has-value? (map |($ :label) table-key-items) ":custom") true)
  (test (has-value? (map |($ :label) table-key-items) "string") false)
  (def table-value-items (complete 175 "{:custom str" 0 12))
  (test (has-value? (map |($ :label) table-value-items) "string") true)
  (def lookup-key-items
    (complete 197 "{:custom 1}\n(get table :cu)" 1 14))
  (test (has-value? (map |($ :label) lookup-key-items) ":custom") true)
  (test (has-value? (map |($ :label) lookup-key-items) "string") false)
  (def metadata-items (complete 176 "(def value :pr" 0 14))
  (test (has-value? (map |($ :label) metadata-items) ":private") true)
  (test (has-value? (map |($ :label) metadata-items) ":prefix") true)
  (def body-keywords (complete 203 "(defn f [] :pr" 0 14))
  (test (has-value? (map |($ :label) body-keywords) ":prefix") true)
  (test (complete 188 "# str" 0 5) @[])
  (test (complete 189 "\"str\"" 0 3) @[])
  (test (complete 190 "`str`" 0 3) @[])
  (def after-long-string (complete 204 "`(use `\nstr" 1 3))
  (test (has-value? (map |($ :label) after-long-string) "string") true)
  (def after-adjacent-long (complete 214 "``a```b`\nstr" 1 3))
  (test (has-value? (map |($ :label) after-adjacent-long) "string") true)

  (def auto-items (complete 177 "# main\nexpo" 1 4))
  (def auto-item
    (first (filter |(= "exported" ($ :label)) auto-items)))
  (test (string/find "./module" (auto-item :detail)) 17)
  (test (get-in auto-item [:additionalTextEdits 0 :newText])
        "(import ./module :only [exported] :prefix \"\")\n")
  (test (get-in auto-item [:additionalTextEdits 0 :range :start :line]) 0)
  (test (get-in auto-item [:additionalTextEdits 0 :range :start :character]) 0)
  (test (get-in auto-item [:textEdit :range :start :line]) 1)
  (test (get-in auto-item [:textEdit :range :start :character]) 0)
  (def shebang-items (complete 198 "#!/usr/bin/env janet\nexpo" 1 4))
  (def shebang-item
    (first (filter |(= "exported" ($ :label)) shebang-items)))
  (test (get-in shebang-item [:additionalTextEdits 0 :range :start :line]) 1)
  (test (get-in shebang-item [:additionalTextEdits 0 :newText])
        "(import ./module :only [exported] :prefix \"\")\n")
  (def spaced-auto-items (complete 213 "#\nspaced" 1 6))
  (def spaced-auto
    (first (filter |(= "spaced-export" ($ :label)) spaced-auto-items)))
  (test (get-in spaced-auto [:additionalTextEdits 0 :newText])
        "(import \"./my module\" :only [spaced-export] :prefix \"\")\n")
  (def nested-import-items
    (complete 205 "(defn f [] (import ./module))\n# top\nexpo" 2 4))
  (def nested-import-item
    (first (filter |(= "exported" ($ :label)) nested-import-items)))
  (test (get-in nested-import-item [:additionalTextEdits 0 :range :start :line]) 0)
  (def late-import-items
    (complete 206 "module/ex\n(import ./module :as module)\n" 0 9))
  (test (has-value? (map |($ :label) late-import-items) "module/exported") false)
  (def nested-alias-items
    (complete 208 "(defn f [] (import ./module :as module))\nmodule/ex" 1 9))
  (test (has-value? (map |($ :label) nested-alias-items) "module/exported") false)
  (test (has-value? (map |($ :label) (complete 209 "string/as" 0 9))
                    "string/ascii-lower")
        true)
  (test (has-value? (map |($ :label) (complete 178 "#\nhid" 1 3)) "hidden") false)
  (test (has-value? (map |($ :label) (complete 191 "#\nsec" 1 3)) "secret") false)
  (test (has-value? (map |($ :label) (complete 192 "#\namb" 1 3)) "ambiguous")
        false)

  (def resolved-current (request cursor 179 "completionItem/resolve" auto-item))
  # The item came from version 177, while later requests advanced the document.
  (test (get-in resolved-current [:result :additionalTextEdits]) nil)

  (def snippet-items (complete 180 "defn" 0 4))
  (def defn-item (first (filter |(= "defn" ($ :label)) snippet-items)))
  (test (get defn-item :insertTextFormat) 2)
  (test (string/has-prefix? "(defn ${1:name}" (defn-item :insertText)) true)
  (def bare-snippets (complete 207 "(" 0 1))
  (def bare-defn (first (filter |(= "defn" ($ :label)) bare-snippets)))
  (test (string/has-prefix? "defn ${1:name}" (bare-defn :insertText)) true)
  (def paired-snippets (complete 215 "()" 0 1))
  (def paired-defn (first (filter |(= "defn" ($ :label)) paired-snippets)))
  (test (string/has-suffix? ")" (paired-defn :insertText)) false)

  (def ranked-source "(defn run [string-local] str)\n")
  (def ranked-items (complete 181 ranked-source 0 28))
  (test (first (map |($ :label) ranked-items)) "string-local")
  (def usage-source "(defn run [alpha alpine] (+ alpha alpha alp))\n")
  (def usage-items (complete 195 usage-source 0 43))
  (test (first (map |($ :label) usage-items)) "alpha")

  (exit-lsp cursor)
  (def plain (spawn-lsp))
  (request plain 193 "initialize"
           {:rootUri root-uri :capabilities {:textDocument {:diagnostic {}}}})
  (open-text-document plain main-uri "defn" 1)
  (def plain-items
    (get-in (request plain 194 "textDocument/completion"
                     {:textDocument {:uri main-uri}
                      :position {:line 0 :character 4}})
            [:result :items]))
  (test (get (first (filter |(= "defn" ($ :label)) plain-items))
             :insertTextFormat)
        nil)
  (exit-lsp plain)
  (when (os/stat cache-path) (os/rm cache-path))
  (remove-tree root))

(deftest "resolve same-named completions in their originating documents"
  (def cursor (start-trusted-lsp {:textDocument {:diagnostic {}}}))
  (def second-uri (uri/path->file-uri
                    (path/abspath "test/resources/format-file-before.txt")))
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet" :version 1
                          :text "(def shared :doc \"from document A\" 1)\nsha\n"}})
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri second-uri :languageId "janet" :version 1
                          :text "(def shared :doc \"from document B\" 2)\nsha\n"}})
  (defn shared-item [id uri]
    (def completion
      (request cursor id "textDocument/completion"
               {:textDocument {:uri uri} :position {:line 1 :character 3}}))
    (first (filter |(= "shared" ($ :label)) (get-in completion [:result :items]))))
  (def item-a (shared-item 86 document-uri))
  (def item-b (shared-item 87 second-uri))
  (change-text-document cursor document-uri
                        "(def shared :doc \"new document A docs\" 3)\nsha\n" 2)
  (def resolved-a (request cursor 88 "completionItem/resolve" item-a))
  (def resolved-b (request cursor 89 "completionItem/resolve" item-b))
  (test (nil? (string/find "from document A"
                           (get-in resolved-a [:result :documentation :value])))
        false)
  (test (string/find "new document A docs"
                     (get-in resolved-a [:result :documentation :value]))
        nil)
  (test (nil? (string/find "from document B"
                           (get-in resolved-b [:result :documentation :value])))
        false)

  (for version 3 8
    (change-text-document cursor document-uri
                          (string "(def shared :doc \"version " version " docs\" "
                                  version ")\nsha\n")
                          version))
  (def evicted (request cursor 149 "completionItem/resolve" item-a))
  (test (get-in evicted [:result :documentation]) nil)

  (notify cursor "textDocument/didClose" {:textDocument {:uri document-uri}})
  (def after-close (request cursor 90 "completionItem/resolve" item-a))
  (test (string/find "from document A"
                     (or (get-in after-close [:result :documentation :value]) ""))
        nil)
  (exit-lsp cursor))

(deftest "format documents without mutating server state"
  (def cursor (start-lsp {:textDocument {:diagnostic {}}}))
  (def source "(def greeting   1)\ngreeting")
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri :languageId "janet"
                          :version 1 :text source}})
  (def params {:textDocument {:uri document-uri}
               :options {:tabSize 2 :insertSpaces true
                         :trimTrailingWhitespace true}})
  (def first-edit (request cursor 73 "textDocument/formatting" params))
  (test (get-in first-edit [:result 0 :range :end :line]) 1)
  (test (get-in first-edit [:result 0 :range :end :character]) 8)
  (test (get-in first-edit [:result 0 :newText]) "(def greeting 1)\ngreeting\n")

  # A rejected edit leaves the same request and analysis result available.
  (def repeated-edit (request cursor 74 "textDocument/formatting" params))
  (test (get-in repeated-edit [:result 0 :newText])
        "(def greeting 1)\ngreeting\n")
  (test (has-value? (completion-labels cursor 75 document-uri 1 4) "greeting") true)

  (def formatted (get-in first-edit [:result 0 :newText]))
  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 2}
           :contentChanges [{:text formatted}]})
  (test (get-in (request cursor 76 "textDocument/formatting" params) [:result]) @[])
  (test (has-value? (completion-labels cursor 77 document-uri 1 4) "greeting") true)

  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 3}
           :contentChanges [{:text "(do \"😀\"   string)"}]})
  (def unicode-edit (request cursor 78 "textDocument/formatting" params))
  (test (get-in unicode-edit [:result 0 :range :end :character]) 18)

  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 4}
           :contentChanges [{:text "("}]})
  (test (get-in (request cursor 79 "textDocument/formatting" params) [:result]) :null)
  (test (get-in (request cursor 80 "janet/serverInfo") [:id]) 80)
  (exit-lsp cursor))

(deftest "format complete ranges and indentation on type"
  (def cursor (start-lsp {:textDocument {:diagnostic {}}}))
  (def source
    (string "(def untouched   1)\n"
            "(defn target [x]\n"
            "  (+   x  1))\n"
            "(def after   2)\n"))
  (open-text-document cursor document-uri source)
  (def range-params
    {:textDocument {:uri document-uri}
     :range {:start {:line 1 :character 0}
             :end {:line 2 :character 13}}
     :options {:tabSize 2 :insertSpaces true}})
  (def ranged (request cursor 252 "textDocument/rangeFormatting" range-params))
  (test (get-in ranged [:result 0 :range :start :line]) 2)
  (test (get-in ranged [:result 0 :range :start :character]) 5)
  (test (get-in ranged [:result 0 :range :end :line]) 2)
  (test (get-in ranged [:result 0 :range :end :character]) 9)
  (test (get-in ranged [:result 0 :newText]) "x")
  (test (length (get-in ranged [:result])) 1)
  # Formatting is pure until the client applies the edit.
  (test (get-in
          (request cursor 253 "textDocument/rangeFormatting" range-params)
          [:result 0 :newText])
        "x")

  (def partial
    (request cursor 254 "textDocument/rangeFormatting"
             {:textDocument {:uri document-uri}
              :range {:start {:line 1 :character 0}
                      :end {:line 2 :character 10}}
              :options {:tabSize 4 :insertSpaces false}}))
  (test (get-in partial [:result 0 :range :start :character]) 5)
  (test (get-in partial [:result 0 :range :end :character]) 9)

  (change-text-document cursor document-uri "(outer   x (foo   bar))\n" 2)
  (def inner
    (request cursor 261 "textDocument/rangeFormatting"
             {:textDocument {:uri document-uri}
              :range {:start {:line 0 :character 11}
                      :end {:line 0 :character 22}}
              :options {:tabSize 2 :insertSpaces true}}))
  (test (length (get-in inner [:result])) 1)
  (test (>= (get-in inner [:result 0 :range :start :character]) 11) true)
  (test (<= (get-in inner [:result 0 :range :end :character]) 22) true)

  (change-text-document cursor document-uri "foo   bar" 3)
  (def atomic
    (request cursor 262 "textDocument/rangeFormatting"
             {:textDocument {:uri document-uri}
              :range {:start {:line 0 :character 0}
                      :end {:line 0 :character 9}}
              :options {:tabSize 2 :insertSpaces true}}))
  (test (length (get-in atomic [:result])) 1)
  (test (get-in atomic [:result 0 :newText]) "")

  (change-text-document cursor document-uri "(foo)   (bar)" 4)
  (def siblings
    (request cursor 263 "textDocument/rangeFormatting"
             {:textDocument {:uri document-uri}
              :range {:start {:line 0 :character 0}
                      :end {:line 0 :character 13}}
              :options {:tabSize 2 :insertSpaces true}}))
  (test (length (get-in siblings [:result])) 1)
  (test (get-in siblings [:result 0 :newText]) "")

  (change-text-document cursor document-uri "(def 😀value   1)\n" 5)
  (def unicode
    (request cursor 255 "textDocument/rangeFormatting"
             {:textDocument {:uri document-uri}
              :range {:start {:line 0 :character 0}
                      :end {:line 0 :character 17}}
              :options {:tabSize 2 :insertSpaces true}}))
  (test (get-in unicode [:result 0 :range :start :character]) 13)
  (test (get-in unicode [:result 0 :range :end :character]) 15)
  (test (get-in unicode [:result 0 :newText]) "")

  (def on-type-source "(defn f []\n        (+ 1 2)\n)\n")
  (change-text-document cursor document-uri on-type-source 6)
  (def on-type-params
    {:textDocument {:uri document-uri}
     :position {:line 1 :character 15}
     :ch ")"
     :options {:tabSize 2 :insertSpaces true}})
  (def on-type (request cursor 256 "textDocument/onTypeFormatting" on-type-params))
  (test (get-in on-type [:result 0 :range :start :line]) 1)
  (test (get-in on-type [:result 0 :range :start :character]) 2)
  (test (get-in on-type [:result 0 :range :end :line]) 2)
  (test (get-in on-type [:result 0 :range :end :character]) 0)
  (test (get-in on-type [:result 0 :newText]) "(+ 1 2)")
  (test (deep=
          (get-in (request cursor 257 "textDocument/onTypeFormatting" on-type-params)
                  [:result])
          (get-in on-type [:result]))
        true)

  (change-text-document cursor document-uri "(" 7)
  (test (get-in
          (request cursor 258 "textDocument/rangeFormatting"
                   {:textDocument {:uri document-uri}
                    :range {:start {:line 0 :character 0}
                            :end {:line 0 :character 1}}
                    :options {:tabSize 2 :insertSpaces true}})
          [:result])
        @[])
  (test (get-in
          (request cursor 259 "textDocument/onTypeFormatting"
                   {:textDocument {:uri document-uri}
                    :position {:line 0 :character 1}
                    :ch ")"
                    :options {:tabSize 2 :insertSpaces true}})
          [:result])
        @[])
  (exit-lsp cursor)

  (def utf8-cursor
    (start-lsp {:general {:positionEncodings ["utf-8"]}
                :textDocument {:diagnostic {}}}))
  (open-text-document utf8-cursor document-uri "(def 😀value   1)\n")
  (def utf8-edit
    (request utf8-cursor 260 "textDocument/rangeFormatting"
             {:textDocument {:uri document-uri}
              :range {:start {:line 0 :character 0}
                      :end {:line 0 :character 19}}
              :options {:tabSize 2 :insertSpaces true}}))
  (test (get-in utf8-edit [:result 0 :range :start :character]) 15)
  (test (get-in utf8-edit [:result 0 :range :end :character]) 17)
  (test (get-in utf8-edit [:result 0 :newText]) "")
  (exit-lsp utf8-cursor))
