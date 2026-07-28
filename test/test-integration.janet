(import spork/json)
(import spork/path)
(import ../src/uri)

(use judge)

(def document-uri (string "file://" (path/abspath "test/resources/format-file-after.txt")))
(def workspace-uri (string "file://" (os/cwd)))
(def document-text "(def greeting (string \"hello\"))\ngreeting\n")

(defn message-frame [message]
  (def body (json/encode message))
  (string "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n"
          "Content-Length: " (length body) "\r\n\r\n" body))

(defn body-frame [body]
  (string "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n"
          "Content-Length: " (length body) "\r\n\r\n" body))

(defn write-output [cursor & messages]
  (each message messages
    (ev/write (cursor :to-lsp) (message-frame message))))

(defn write-chunked [cursor message]
  (def frame (message-frame message))
  (for i 0 (length frame)
    (ev/write (cursor :to-lsp) (string/slice frame i (inc i)))))

(defn write-body [cursor body]
  (ev/write (cursor :to-lsp) (body-frame body)))

(defn read-output [cursor]
  (def headers @"")
  (while (not (string/has-suffix? "\r\n\r\n" headers))
    (def chunk (ev/read (cursor :from-lsp) 1))
    (unless chunk (error "language server closed stdout in response headers"))
    (buffer/push-string headers chunk))
  (unless (string/has-prefix? "Content-Length:" headers)
    (error "language server wrote non-protocol data to stdout"))

  (def content-length-line
    (first (filter |(string/has-prefix? "Content-Length:" $)
                   (string/split "\r\n" headers))))
  (unless content-length-line (error "language server response has no Content-Length"))
  (def content-length
    (scan-number (string/trim
                   (string/slice content-length-line (length "Content-Length:")))))

  (def body @"")
  (while (< (length body) content-length)
    (def chunk (ev/read (cursor :from-lsp) (- content-length (length body))))
    (unless chunk (error "language server returned a truncated response"))
    (buffer/push-string body chunk))
  (json/decode body true))

(defn request [cursor id method &opt params]
  (write-output cursor {:jsonrpc "2.0" :id id :method method :params (or params {})})
  (read-output cursor))

(defn notify [cursor method &opt params]
  (write-output cursor {:jsonrpc "2.0" :method method :params (or params {})}))

(defn respond [cursor id result]
  (write-output cursor {:jsonrpc "2.0" :id id :result result}))

(defn completion-labels [cursor id document-uri line character]
  (def response
    (request cursor id "textDocument/completion"
             {:textDocument {:uri document-uri}
              :position {:line line :character character}}))
  (map |($ :label) (get-in response [:result :items])))

(defn spawn-lsp []
  (def janet-lsp
    (os/spawn [(dyn :executable) "./src/main.janet" "--dont-search-jpm-tree"]
              :p {:in :pipe :out :pipe}))
  @{:process janet-lsp
    :to-lsp (janet-lsp :in)
    :from-lsp (janet-lsp :out)})

(defn start-lsp [&opt capabilities initialization-options]
  (default capabilities {})
  (def cursor (spawn-lsp))
  (put cursor :initialize
       (request cursor 0 "initialize"
                {:rootUri workspace-uri
                 :capabilities capabilities
                 :initializationOptions (or initialization-options {})}))
  cursor)

(defn start-trusted-lsp [&opt capabilities]
  (start-lsp capabilities {:trustedWorkspaces [workspace-uri]}))

(defn exit-lsp [cursor]
  (request cursor 99 "shutdown")
  (notify cursor "exit")
  (os/proc-wait (cursor :process)))

(defn open-document [cursor]
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri document-uri
                          :languageId "janet"
                          :version 1
                          :text document-text}})
  (read-output cursor))

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
  (test (get-in (request cursor 1 "janet/serverInfo") [:result :serverInfo :name])
        "janet-lsp"))

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

(deftest "reject requests before initialization"
  (def cursor (spawn-lsp))
  (def response (request cursor 30 "janet/serverInfo"))
  (test (get-in response [:error :code]) -32002)
  (test (get-in (request cursor 31 "initialize" {:capabilities {}}) [:id]) 31)
  (request cursor 32 "shutdown")
  (notify cursor "exit")
  (test (os/proc-wait (cursor :process)) 0))

(deftest "exit without shutdown is immediate and unsuccessful"
  (def cursor (spawn-lsp))
  (notify cursor "exit")
  (test (os/proc-wait (cursor :process)) 1))

(deftest "workspace startup requires explicit client trust"
  (def workspace (string "/tmp/janet-lsp-trust-" (os/getpid)))
  (def config-dir (path/join workspace ".janet-lsp"))
  (def startup (path/join config-dir "startup.janet"))
  (def marker (path/join workspace "startup-ran"))
  (def root-uri (string "file://" workspace))
  (os/mkdir workspace)
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
  (os/proc-wait (untrusted :process))

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
  (os/proc-wait (trusted :process))

  (os/rm marker)
  (os/rm startup)
  (os/rmdir config-dir)
  (os/rmdir workspace))

(deftest "scope analysis across workspace folder changes"
  (def base (string "/tmp/janet-lsp-workspaces-" (os/getpid)))
  (def root-a (path/join base "a"))
  (def root-b (path/join base "b"))
  (def root-c (path/join base "c"))
  (each root [base root-a root-b root-c]
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
  (each file [file-a file-b
              (path/join root-a ".janet-lsp" "startup.janet")
              (path/join root-b ".janet-lsp" "startup.janet")
              (path/join root-c ".janet-lsp" "startup.janet")]
    (os/rm file))
  (each root [root-a root-b root-c]
    (os/rmdir (path/join root ".janet-lsp"))
    (os/rmdir root))
  (os/rmdir base))

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
  (test (os/proc-wait (cursor :process)) 0))

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
  (test (= (get-in definition [:result :uri]) document-uri) true)
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
  (test (get-in definition [:result]) :null))

(deftest: with-process-open "document change publishes diagnostics" [cursor]
  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 2}
           :contentChanges [{:text document-text}]})
  (def response (read-output cursor))
  (test (= (get-in response [:params :uri]) document-uri) true)
  (test (get-in response [:params :version]) 2)
  (test (get-in response [:params :diagnostics]) @[]))

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
                          :version 4 :text "(def second 1)\nsecond\n"}})
  (read-output cursor)
  (notify cursor "textDocument/didChange"
          {:textDocument {:uri second-uri :version 5}
           :contentChanges [{:text "(def second 2)\nsecond\n"}]})
  (test (get-in (read-output cursor) [:params :version]) 5)

  (notify cursor "textDocument/didClose" {:textDocument {:uri second-uri}})
  (def cleared (read-output cursor))
  (test (= (get-in cleared [:params :uri]) second-uri) true)
  (test (get-in cleared [:params :diagnostics]) @[])
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

(deftest: with-process-open "pull diagnostics returns a full report" [cursor]
  (def response
    (request cursor 3 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (get-in response [:result :kind]) "full")
  (test (get-in response [:result :items]) @[]))

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
           :contentChanges [{:text (string/repeat " " 1048577)}]})
  (def bounded-report
    (request cursor 72 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (string/has-prefix? "analysis limit exceeded:"
                            (get-in bounded-report [:result :items 0 :message]))
        true)
  (test (get-in (request cursor 69 "janet/serverInfo") [:id]) 69)
  (exit-lsp cursor))

(deftest: with-process "cancel pull diagnostic requests" [cursor]
  (notify cursor "$/cancelRequest" {:id 70})
  (def cancelled
    (request cursor 70 "textDocument/diagnostic"
             {:textDocument {:uri document-uri}}))
  (test (get-in cancelled [:error :code]) -32800)
  (test (get-in (request cursor 71 "janet/serverInfo") [:id]) 71))

(deftest: with-process-open "completion includes core and local bindings" [cursor]
  (def response
    (request cursor 4 "textDocument/completion"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 4}
              :context {:triggerKind 1}}))
  (def labels (map |($ :label) (get-in response [:result :items])))
  (test (get-in response [:result :isIncomplete]) true)
  (test (has-value? labels "string") true)
  (test (has-value? labels "greeting") true))

(deftest: with-process-open "completion items resolve documentation" [cursor]
  (def completion
    (request cursor 5 "textDocument/completion"
             {:textDocument {:uri document-uri}
              :position {:line 1 :character 4}
              :context {:triggerKind 1}}))
  (def item (first (filter |(= "string" ($ :label))
                           (get-in completion [:result :items]))))
  (def response (request cursor 6 "completionItem/resolve" item))
  (test (get-in response [:result :label]) "string")
  (test (get-in response [:result :documentation :kind]) "markdown")
  (test (string/has-prefix? "cfunction"
                            (get-in response [:result :documentation :value]))
        true))
