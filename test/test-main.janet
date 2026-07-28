(use judge)

(import ../src/main)
(import ../src/analysis)
(import ../src/documents)
(import ../src/editor-features)
(import ../src/eval)
(import ../src/logging)
(import ../src/lint)
(import ../src/index)
(import ../src/position)
(import ../src/transport)
(import ../src/uri)
(import ../src/server-utils)
(import ../src/signatures)
(import ../src/workspace)
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

(deftest "apply validated incremental document changes atomically"
  (def source "a😀b\nsecond")
  (test (documents/apply-changes
          source
          [{"range" {"start" {"line" 0 "character" 1}
                       "end" {"line" 0 "character" 3}}
            "rangeLength" 2
            "text" "X"}
           {"range" {"start" {"line" 1 "character" 0}
                       "end" {"line" 1 "character" 6}}
            "rangeLength" 6
            "text" "next"}]
          "utf-16")
        "aXb\nnext")
  (test (documents/apply-changes
          source
          [{"range" {"start" {"line" 0 "character" 1}
                       "end" {"line" 0 "character" 5}}
            "rangeLength" 4
            "text" "X"}]
          "utf-8")
        "aXb\nsecond")
  (test (documents/apply-changes source
                                 [{"range" {"start" {"line" 0 "character" 1}
                                            "end" {"line" 0 "character" 3}}
                                   "rangeLength" 1
                                   "text" "X"}]
                                 "utf-16")
        nil)
  (test (documents/apply-changes
          source
          [{"range" {"start" {"line" 0 "character" 0}
                       "end" {"line" 0 "character" 1}}
            "text" "changed"}
           {"range" {"start" {"line" 9 "character" 0}
                       "end" {"line" 9 "character" 1}}
            "text" "invalid"}]
          "utf-16")
        nil)
  (test (documents/apply-changes source [{"text" "replacement"}] "utf-16")
        "replacement"))

(deftest "bound versioned document snapshot caches"
  (def document @{:content "value" :version 1})
  (for version 1 7
    (analysis/store document {:key (string version ":snapshot")
                              :version version
                              :eval-env (make-env root-env)}))
  (test (length (document :snapshots)) 4)
  (test (document :snapshot-order)
        @["3:snapshot" "4:snapshot" "5:snapshot" "6:snapshot"])
  (analysis/invalidate document)
  (test (document :analysis) nil)
  (test (document :snapshots) @{})
  (def workspace @{:uri "file:///workspace" :trusted false})
  (analysis/store document {:key (analysis/key 1 "value")
                            :version 1
                            :workspace-uri "file:///workspace"
                            :trusted false
                            :eval-env (make-env root-env)})
  (test (not (nil? (analysis/current document workspace))) true)
  (put workspace :trusted true)
  (test (analysis/current document workspace) nil))

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

(deftest "index multiline syntax and resolve module identities"
  (def multiline
    (index/analyze
      "file:///workspace/multiline.janet"
      "(defn\n  outer :doc \"docs\"\n  [[a b] &opt c]\n  (defn inner [x] x)\n  (+ a c))\n"))
  (test (map |($ :name) (multiline :definitions)) @["outer" "inner"])
  (test (map |($ :name) (get-in multiline [:definitions 0 :children]))
        @["a" "b" "c"])
  (test (get-in multiline [:definitions 0 :range :end :line]) 4)
  (test (= (get-in multiline [:definitions 1 :container])
           (get-in multiline [:definitions 0 :identity]))
        true)

  (def workspace @{:index @{}})
  (index/update workspace "file:///workspace/value.janet"
                "(def shared 1)\nshared\n")
  (index/update workspace "file:///workspace/middle.janet"
                "(use ./value :only [shared] :export true)\n")
  (index/update workspace "file:///workspace/main.janet"
                "(import ./middle :as module)\nmodule/shared\n")
  (def definition
    (index/resolve-definition workspace "file:///workspace/main.janet"
                              "module/shared"))
  (test (definition :uri) "file:///workspace/value.janet")
  (test (length (index/references-by-identity workspace (definition :identity))) 4)
  (index/update workspace "file:///workspace/value.janet" "(def replacement 1)\n")
  (test (index/references-by-identity workspace (definition :identity)) @[])

  (def generated-source
    "(defmacro make-value [] ~(def generated-value 1))\n(make-value)\n")
  (def [_ generated-env]
    (eval/eval-buffer generated-source "/tmp/generated-index.janet" {:trusted true}))
  (index/update workspace "file:///tmp/generated-index.janet" generated-source)
  (index/add-generated workspace "file:///tmp/generated-index.janet" generated-env)
  (test (get-in (first (index/definitions workspace "generated-value")) [:generated])
        true))

(deftest "warn only for provably unused function parameters"
  (def diagnostics
    (lint/analyze "(defn run [used unused _ignored] (+ used 1))\n"))
  (test (map |($ :message) diagnostics) @["unused parameter unused"])
  (test (get-in diagnostics [0 :code]) "janet.lint.unused-parameter")
  (test (lint/analyze "(defn run [{:keys [value]}] value)\n") @[])
  (test (map |($ :message) (lint/analyze "(defn run [[a b]] nil)\n"))
        @["unused parameter a" "unused parameter b"])
  (test (lint/analyze
          "(defmacro use-value [] 'value)\n(defn run [value] (use-value))\n")
        @[]))

(deftest "validate safe positional and named calls"
  (def source
    (string "(defn exact [a] nil)\n"
            "(defn run [required &opt optional &named option] nil)\n"
            "(exact)\n"
            "(exact 1 2)\n"
            "(run 1 2 :unknown 3)\n"
            "(run 1 2 :option 3 :option 4)\n"
            "(run 1 2 :option)\n"
            "(defn caller [run] (run))\n"))
  (test (map |($ :code) (signatures/diagnostics source))
        @["janet.call.missing-arguments"
          "janet.call.extra-arguments"
          "janet.call.unknown-named-argument"
          "janet.call.duplicate-named-argument"
          "janet.call.odd-named-arguments"])
  (def signature (signatures/find source "run"))
  (test (signature :label) "(run required &opt optional &named option)")
  (test (map |($ :label) (signature :parameters))
        @["required" "optional" ":option"])
  (def conservative
    (string "(defn optional [a &opt b] nil)\n(optional 1)\n"
            "(defn variadic [a & rest] nil)\n(variadic)\n(variadic 1 2)\n"
            "(defn destruct [[a b]] nil)\n(destruct)\n(destruct [1 2])\n"
            "(defn exact [a] nil)\n(exact ;args)\n"))
  (test (map |($ :code) (signatures/diagnostics conservative))
        @["janet.call.missing-arguments"
          "janet.call.missing-arguments"])
  (test (signatures/find
          "(defn duplicate [a] nil)\n(defn duplicate [a b] nil)\n"
          "duplicate")
        nil))

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

  (test (map (juxt 1 |(editor-features/binding-to-lsp-item (first $) eval-env)) test-cases)
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
  (def files (workspace/find-all-module-files (path/join (os/cwd) "src")))
  (def basenames (map path/basename files))
  (test (has-value? basenames "main.janet") true)
  (test (has-value? basenames "parser.janet") true)
  (test (all |(or (string/has-suffix? ".janet" $)
                  (string/has-suffix? ".jimage" $)
                  (string/has-suffix? ".so" $)) files)
        true))

(deftest "find unique module paths"
  (def paths (workspace/find-unique-paths
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
  (test (get (server-utils/workspace-for-path state (path/join nested "main.janet")) :uri)
        "nested")
  (test (get (server-utils/workspace-for-path state (path/join root "main.janet")) :uri)
        "root")
  (test (get (server-utils/workspace-for-path state (path/join (os/cwd) "other" "main.janet")) :uri)
        "standalone"))

(deftest "derive initialization workspace roots"
  (test (workspace/initialization-uris
          {"rootUri" "file:///root"
           "workspaceFolders" [{"uri" "file:///a"} {"uri" "file:///b"}]})
        @["file:///a" "file:///b"])
  (test (workspace/initialization-uris {"rootUri" "file:///root"})
        ["file:///root"])
  (test (first (workspace/initialization-uris {"rootPath" "/tmp/root"}))
        "file:///tmp/root"))
