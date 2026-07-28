(use judge)

(import ../src/main)
(import ../src/transport)
(import spork/path)

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

(test (peg/match main/uri-percent-encoding-peg "file:///c%3A/Users/pete/Desktop/code/libmpsse")
      @["file:///c:/Users/pete/Desktop/code/libmpsse"])

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
  (test paths
        @["./src/:all:.janet"
          "./example/:all:.janet"
          "./example/init.janet"]))
