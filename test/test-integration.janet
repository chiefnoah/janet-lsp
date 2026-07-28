(import ../libs/jayson)
(import spork/path)

(use judge)

(def document-uri (string "file://" (path/abspath "test/resources/format-file-after.txt")))
(def document-text "(def greeting (string \"hello\"))\ngreeting\n")

(defn write-output [cursor & messages]
  (each message messages
    (def body (jayson/encode message))
    (ev/write (cursor :to-lsp)
              (string "Content-Length: " (length body) "\r\n\r\n" body))))

(defn read-output [cursor]
  (def headers @"")
  (while (not (string/has-suffix? "\r\n\r\n" headers))
    (def chunk (ev/read (cursor :from-lsp) 1))
    (unless chunk (error "language server closed stdout in response headers"))
    (buffer/push-string headers chunk))

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
  (jayson/decode body true))

(defn request [cursor id method &opt params]
  (write-output cursor {:jsonrpc "2.0" :id id :method method :params (or params {})})
  (read-output cursor))

(defn notify [cursor method &opt params]
  (write-output cursor {:jsonrpc "2.0" :method method :params (or params {})}))

(defn start-lsp []
  (def janet-lsp
    (os/spawn [(dyn :executable) "./src/main.janet" "--dont-search-jpm-tree"]
              :p {:in :pipe :out :pipe}))
  (def cursor @{:process janet-lsp
                :to-lsp (janet-lsp :in)
                :from-lsp (janet-lsp :out)})
  (put cursor :initialize
       (request cursor 0 "initialize"
                {:rootUri (string "file://" (os/cwd)) :capabilities {}}))
  cursor)

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
  (test (get-in initialize [:result :capabilities :completionProvider :resolveProvider]) true)
  (test (get-in (request cursor 1 "janet/serverInfo") [:result :serverInfo :name])
        "janet-lsp"))

(deftest: with-process-open "document open publishes diagnostics" [cursor]
  (test (= (get-in (cursor :open) [:params :uri]) document-uri) true)
  (test (get-in (cursor :open) [:params :diagnostics]) @[]))

(deftest: with-process-open "document change publishes diagnostics" [cursor]
  (notify cursor "textDocument/didChange"
          {:textDocument {:uri document-uri :version 2}
           :contentChanges [{:text document-text}]})
  (def response (read-output cursor))
  (test (= (get-in response [:params :uri]) document-uri) true)
  (test (get-in response [:params :diagnostics]) @[]))

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
