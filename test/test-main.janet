(use judge)

(import ../src/main)
(import ../src/eval)
(import ../src/logging)
(import ../src/index)
(import ../src/position)
(import ../src/transport)
(import ../src/uri)
(import spork/json)
(import spork/path)

(deftest "decode strict JSON"
  (test (json/decode "1e3") 1000)
  (test (json/decode "null") :null)
  (test-error (json/decode "Null")
              "decode error at position 0: unexpected character")
  (test-error (json/decode "{} trailing")
              "decode error at position 3: unexpected extra token"))

(deftest "select configured debug ports"
  (test (logging/debug-port nil) "8037")
  (test (logging/debug-port {:debug-port 9123}) "9123"))

(deftest "index definitions and code references"
  (def record
    (index/analyze "file:///workspace/main.janet"
                   "(def value 1)\n(defn run [x] (+ value x))\n# value\n\"value\"\n"))
  (test (map |($ :name) (get record :definitions)) @["value" "run"])
  (test (length (filter |(= "value" ($ :name)) (get record :references))) 2)
  (def workspace @{:index @{}})
  (index/update workspace "file:///workspace/main.janet" "(def value 1)")
  (test (length (index/definitions workspace "value")) 1)
  (index/remove workspace "file:///workspace/main.janet")
  (test (index/definitions workspace "value") @[]))

(deftest "convert negotiated position encodings"
  (def line "aé☃😀é")
  (test (map |(position/units-to-byte line $ "utf-16") [0 1 2 3 5 6 7])
        @[0 1 3 6 10 11 13])
  (test (map |(position/byte-to-units line $ "utf-16") [0 1 3 6 10 11 13])
        @[0 1 2 3 5 6 7])
  (test (map |(position/units-to-byte line $ "utf-8") [0 1 3 6 10 11 13])
        @[0 1 3 6 10 11 13])
  (test (map |(position/units-to-byte line $ "utf-32") [0 1 2 3 4 5 6])
        @[0 1 3 6 10 11 13]))

(deftest "convert multiline positions and reject invalid offsets"
  (def source "ascii\naé😀\n")
  (test (position/lsp->byte-position source {:line 1 :character 4} "utf-16")
        {:line 1 :character 7})
  (test (position/byte->lsp-position source {:line 1 :character 7} "utf-16")
        {:line 1 :character 4})
  (test (position/document-end source "utf-16") {:line 2 :character 0})
  (test (position/lsp->byte-position source {:line 1 :character 2} "utf-16")
        {:line 1 :character 3})
  (test (position/lsp->byte-position source {:line 1 :character 3} "utf-16") nil)
  (test (position/lsp->byte-position source {:line 3 :character 0} "utf-16") nil)
  (test (position/lsp->byte-position source {:line 1 :character 5} "utf-16") nil)
  (test (position/byte->lsp-position source {:line 1 :character 2} "utf-16") nil))

(deftest "parse LSP headers"
  (test (transport/parse-headers ["Content-Length: 123\r\n"]) 123)
  (test (transport/parse-headers ["Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n"
                                  "content-length: 42\r\n"])
        42)
  (test-error (transport/parse-headers ["Content-Type: application/json"])
              "malformed LSP headers: missing Content-Length")
  (test-error (transport/parse-headers ["Content-Length: nope"])
              "malformed LSP headers: invalid Content-Length")
  (test-error (transport/parse-headers ["Content-Length: 1" "Content-Length: 2"])
              "malformed LSP headers: duplicate Content-Length"))

(deftest "read consecutive LSP frames"
  (def stream (file/temp))
  (file/write stream "Content-Type: application/json\r\nContent-Length: 3\r\n\r\none"
                     "Content-Length: 3\r\n\r\ntwo")
  (file/seek stream :set 0)
  (test (transport/read-frame stream) @"one")
  (test (transport/read-frame stream) @"two")
  (test (transport/read-frame stream) nil)
  (file/close stream))

(deftest "reject truncated LSP frames"
  (def stream (file/temp))
  (file/write stream "Content-Length: 4\r\n\r\ntwo")
  (file/seek stream :set 0)
  (test-error (transport/read-frame stream)
              "truncated LSP body: expected 4 bytes, received 3")
  (file/close stream))

(deftest "write LSP frames with CRLF"
  (def stream (file/temp))
  (transport/write-frame stream "body")
  (file/seek stream :set 0)
  (test (file/read stream :all) @"Content-Length: 4\r\n\r\nbody")
  (file/close stream))

(deftest "convert file URIs and paths"
  (test (uri/file-uri->path "file:///tmp/a%20b%25%23%3F%E2%98%83.janet")
        "/tmp/a b%#?☃.janet")
  (test (uri/path->file-uri "/tmp/a b%#?☃.janet")
        "file:///tmp/a%20b%25%23%3F%E2%98%83.janet")
  (test (uri/file-uri->path "file:///C:/Users/Janet%20User/main.janet")
        "C:/Users/Janet User/main.janet")
  (test (uri/path->file-uri "C:\\Users\\Janet User\\main.janet")
        "file:///C:/Users/Janet%20User/main.janet")
  (test (uri/file-uri->path "file://server/share/a%20b.janet")
        "//server/share/a b.janet")
  (test (uri/path->file-uri "//server/share/a b.janet")
        "file://server/share/a%20b.janet")
  (test (uri/path->file-uri "/tmp/[]@!$&'()*+,;=.janet")
        "file:///tmp/%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D.janet"))

(deftest "reject non-file and malformed URIs"
  (test (uri/file-uri->path "untitled:buffer") nil)
  (test (uri/file-uri->path "file:///tmp/a#fragment") nil)
  (test-error (uri/file-uri->path "file:///tmp/%GG")
              "invalid percent escape in URI"))

(deftest "untrusted analysis does not execute workspace code"
  (def prefix (string "/tmp/janet-lsp-safe-analysis-" (os/getpid)))
  (def macro-marker (string prefix "-macro"))
  (def import-marker (string prefix "-import"))
  (def flycheck-marker (string prefix "-flycheck"))
  (def imported-file (string prefix "-imported.janet"))
  (spit imported-file
        (string "(defn imported-run :flycheck [] (spit "
                (string/format "%q" import-marker) " \"ran\"))\n"
                "(imported-run)\n"))

  (def macro-source
    (string "(defmacro run-macro [] (spit " (string/format "%q" macro-marker) " \"ran\"))\n"
            "(run-macro)\n"))
  (def import-source (string "(dofile " (string/format "%q" imported-file) ")\n"))
  (def flycheck-source
    (string "(defn run-flycheck :flycheck [] (spit "
            (string/format "%q" flycheck-marker) " \"ran\"))\n"
            "(run-flycheck)\n"))
  (each source [macro-source import-source flycheck-source]
    (eval/eval-buffer source "safe-test.janet"))
  (test (os/stat macro-marker) nil)
  (test (os/stat import-marker) nil)
  (test (os/stat flycheck-marker) nil)

  (each source [macro-source import-source flycheck-source]
    (eval/eval-buffer source "trusted-test.janet" {:trusted true}))
  (test (os/stat macro-marker :mode) :file)
  (test (os/stat import-marker :mode) :file)
  (test (os/stat flycheck-marker :mode) :file)

  (each file [macro-marker import-marker flycheck-marker imported-file]
    (when (os/stat file) (os/rm file))))

(deftest "test binding-to-lsp-item"
  (def eval-env (table/proto-flatten (make-env root-env)))

  (def bind-fiber (fiber/new |(do (defglobal "anil" nil)
                                (defglobal "hello" 'world)
                                (defglobal "atuple" [:a 1])
                                true) :e eval-env))
  (def bf-return (resume bind-fiber))

  (def test-cases @[['hello :symbol] [true :boolean] [% :function]
                    [abstract? :cfunction] ["Hello world" :string]
                    [@"Hello world" :buffer] [123 :number]
                    [:keyword :keyword] [stderr :core/file]
                    [(peg/compile 1) :core/peg] [{:a 1} :struct]
                    [@{:a 1} :table] ['atuple :tuple]
                    [@[:a 1] :array] # [(coro) :fiber]
                    ['anil :nil]])

  (test (map (juxt 1 |(main/binding-to-lsp-item (first $) eval-env)) test-cases)
        @[[:symbol {:kind 12 :label hello}]
          [:boolean {:kind 6 :label true}]
          [:function {:kind 3 :label @%}]
          [:cfunction {:kind 3 :label @abstract?}]
          [:string {:kind 6 :label "Hello world"}]
          [:buffer {:kind 6 :label @"Hello world"}]
          [:number {:kind 6 :label 123}]
          [:keyword {:kind 6 :label :keyword}]
          [:core/file {:kind 17 :label "<core/file 0x1>"}]
          [:core/peg {:kind 6 :label "<core/peg 0x2>"}]
          [:struct {:kind 6 :label {:a 1}}]
          [:table {:kind 6 :label @{:a 1}}]
          [:tuple {:kind 6 :label atuple}]
          [:array {:kind 6 :label @[:a 1]}]
          [:nil {:kind 12 :label anil}]]))

(deftest "find module files without machine-specific paths"
  (def files (main/find-all-module-files (path/join (os/cwd) "src")))
  (def basenames (map path/basename files))
  (test (has-value? basenames "main.janet") true)
  (test (has-value? basenames "parser.janet") true)
  (test (all |(or (string/has-suffix? ".janet" $)
                  (string/has-suffix? ".jimage" $)
                  (string/has-suffix? ".so" $)) files)
        true))

(deftest "find unique module paths"
  (def paths (main/find-unique-paths
               [(path/join (os/cwd) "src/main.janet")
                (path/join (os/cwd) "src/parser.janet")
                (path/join (os/cwd) "example/init.janet")]))
  (test (map |(path/relpath (os/cwd) $) paths)
        @["src/:all:.janet"
          "example/:all:.janet"
          "example/init.janet"]))

(deftest "select the most specific owning workspace"
  (def root (path/join (os/cwd) "workspace"))
  (def nested (path/join root "nested"))
  (def state {:workspaces {"root" {:uri "root" :path root}
                           "nested" {:uri "nested" :path nested}}
              :standalone-workspace {:uri "standalone" :path nil}})
  (test (get (main/workspace-for-path state (path/join nested "main.janet")) :uri)
        "nested")
  (test (get (main/workspace-for-path state (path/join root "main.janet")) :uri)
        "root")
  (test (get (main/workspace-for-path state (path/join (os/cwd) "other" "main.janet")) :uri)
        "standalone"))

(deftest "derive initialization workspace roots"
  (test (main/initialization-workspace-uris
          {"rootUri" "file:///root"
           "workspaceFolders" [{"uri" "file:///a"} {"uri" "file:///b"}]})
        @["file:///a" "file:///b"])
  (test (main/initialization-workspace-uris {"rootUri" "file:///root"})
        ["file:///root"])
  (test (first (main/initialization-workspace-uris {"rootPath" "/tmp/root"}))
        "file:///tmp/root"))
