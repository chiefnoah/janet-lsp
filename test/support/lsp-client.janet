(import spork/json)
(import spork/path)
(import ../../src/index-cache)
(import ../../src/uri)

(def document-uri
  (uri/path->file-uri (path/abspath "test/resources/format-file-after.txt")))
(def workspace-uri (uri/path->file-uri (os/cwd)))
(def document-text "(def greeting (string \"hello\"))\ngreeting\n")

(varfn remove-tree [])

(defn- body-frame [body]
  (string "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n"
          "Content-Length: " (length body) "\r\n\r\n" body))

(defn message-frame [message]
  (body-frame (json/encode message)))

(defn write-output [cursor & messages]
  (each message messages
    (ev/write (cursor :to-lsp) (message-frame message))))

(defn write-chunked [cursor message]
  (def frame (message-frame message))
  (for index 0 (length frame)
    (ev/write (cursor :to-lsp) (string/slice frame index (inc index)))))

(defn write-body [cursor body]
  (ev/write (cursor :to-lsp) (body-frame body)))

(defn read-output [cursor]
  (def headers @"")
  (while (not (string/has-suffix? "\r\n\r\n" headers))
    (def chunk (ev/read (cursor :from-lsp) 1 nil 10))
    (unless chunk (error "language server closed stdout in response headers"))
    (buffer/push-string headers chunk))
  (unless (string/has-prefix? "Content-Length:" headers)
    (error "language server wrote non-protocol data to stdout"))
  (def content-length-line
    (first (filter |(string/has-prefix? "Content-Length:" $)
                   (string/split "\r\n" headers))))
  (unless content-length-line
    (error "language server response has no Content-Length"))
  (def content-length
    (scan-number
      (string/trim
        (string/slice content-length-line (length "Content-Length:")))))
  (def body @"")
  (while (< (length body) content-length)
    (def chunk (ev/read (cursor :from-lsp)
                        (- content-length (length body)) nil 10))
    (unless chunk (error "language server returned a truncated response"))
    (buffer/push-string body chunk))
  (json/decode body true))

(defn request [cursor id method &opt params]
  (write-output cursor {:jsonrpc "2.0" :id id :method method
                        :params (or params {})})
  (read-output cursor))

(defn notify [cursor method &opt params]
  (write-output cursor {:jsonrpc "2.0" :method method :params (or params {})}))

(defn respond [cursor id result]
  (write-output cursor {:jsonrpc "2.0" :id id :result result}))

(defn completion-labels [cursor id uri line character]
  (def response
    (request cursor id "textDocument/completion"
             {:textDocument {:uri uri}
              :position {:line line :character character}}))
  (map |($ :label) (get-in response [:result :items])))

(defn spawn-lsp []
  (def process
    (os/spawn [(dyn :executable) "./src/main.janet" "--dont-search-jpm-tree"]
              :p {:in :pipe :out :pipe}))
  @{:process process :to-lsp (process :in) :from-lsp (process :out)})

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

(defn wait-process [process]
  (try (ev/with-deadline 10 (os/proc-wait process))
    ([err]
      (try (os/proc-kill process true) ([_] nil))
      (error (string "language server did not exit: " err)))))

(defn exit-lsp [cursor]
  (def graceful?
    (try
      (do (request cursor 99 "shutdown")
          (notify cursor "exit")
          true)
      ([_] false)))
  (unless graceful?
    (try (os/proc-kill (cursor :process) true) ([_] nil)))
  (wait-process (cursor :process)))

(defn open-text-document [cursor uri text &opt version]
  (default version 1)
  (notify cursor "textDocument/didOpen"
          {:textDocument {:uri uri :languageId "janet"
                          :version version :text text}}))

(defn change-text-document [cursor uri text version]
  (notify cursor "textDocument/didChange"
          {:textDocument {:uri uri :version version}
           :contentChanges [{:text text}]}))

(defn close-text-document [cursor uri]
  (notify cursor "textDocument/didClose" {:textDocument {:uri uri}}))

(defn open-document [cursor]
  (open-text-document cursor document-uri document-text)
  (read-output cursor))

(defn temp-directory [name]
  (def root (path/join "/tmp" (string name "-" (os/getpid))))
  (when (os/stat root) (remove-tree root))
  (os/mkdir root)
  root)

(varfn remove-tree [root]
  (case (os/stat root :mode)
    :directory
    (do
      (def cache-path (index-cache/path-for (uri/path->file-uri root)))
      (when (os/stat cache-path) (os/rm cache-path))
      (each entry (os/dir root)
        (remove-tree (path/join root entry)))
      (os/rmdir root))
    :file (os/rm root)
    nil))
