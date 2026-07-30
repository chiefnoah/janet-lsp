(use judge)

(import ../src/main)
(import ../src/analysis)
(import ../src/documents)
(import ../src/editor-features)
(import ../src/eval)
(import ../src/logging)
(import ../src/lint)
(import ../src/index)
(import ../src/index-cache)
(import ../src/configuration)
(import ../src/parser)
(import ../src/platform)
(import ../src/position)
(import ../src/transport)
(import ../src/uri)
(import ../src/server-utils)
(import ../src/semantic-tokens)
(import ../src/signatures)
(import ../src/static-diagnostics)
(import ../src/workspace)
(import spork/json)
(import spork/path)

(varfn remove-test-tree [])

(varfn remove-test-tree [target]
  (case (os/stat target :mode)
    :directory
    (do
      (each entry (os/dir target)
        (remove-test-tree (path/join target entry)))
      (os/rmdir target))
    (when (os/stat target) (os/rm target))))

(deftest "decode strict JSON"
  (test (json/decode "1e3") 1000)
  (test (json/decode "null") :null)
  (test-error (json/decode "Null")
              "decode error at position 0: unexpected character")
  (test-error (json/decode "{} trailing")
              "decode error at position 3: unexpected extra token"))

(deftest "match positions against internal and protocol ranges"
  (def position {:line 8 :character 4})
  (test (server-utils/position-in-range?
          position
          {:start {:line 8 :character 2} :end {:line 8 :character 6}})
        true)
  (test (server-utils/position-in-range?
          position
          @{"start" @{"line" 8 "character" 2}
            "end" @{"line" 8 "character" 6}})
        true)
  (test (server-utils/position-in-range?
          position
          {:start {:line 0 :character 8} :end {:line 0 :character 19}})
        false))

(deftest "compute exact semantic token deltas"
  (def previous @[0 0 3 4 0 0 4 2 4 0])
  (def current @[0 0 3 4 0 0 4 5 2 1 0 6 2 4 0])
  (def edits (semantic-tokens/delta previous current))
  (test (length edits) 1)
  (def edit (edits 0))
  (def applied
    (array
      ;(array/slice previous 0 (edit :start))
      ;(edit :data)
      ;(array/slice previous (+ (edit :start) (edit :deleteCount)))))
  (test (deep= applied current) true)
  (test (semantic-tokens/delta current current) @[]))

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
  (test (has-value? index/default-exclusions ".direnv") true)
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

(deftest "keep unchanged cached records when installing document analysis"
  (def document-uri "file:///workspace/main.janet")
  (def source "(def cached-value 1)\n")
  (def cached (index/analyze document-uri source))
  (def workspace @{:index @{document-uri cached} :index-generation 3})
  (analysis/replace-record workspace document-uri (index/analyze document-uri source))
  (test (= (get-in workspace [:index document-uri]) cached) true)
  (test (workspace :index-generation) 3)
  (array/push (cached :definitions)
              {:name "generated" :generated true :uri document-uri})
  (analysis/replace-record workspace document-uri (index/analyze document-uri source))
  (test (= (get-in workspace [:index document-uri]) cached) false)
  (test (index/definitions workspace "generated") @[]))

(deftest "honor git ignores and hidden directories during source discovery"
  (def root (platform/temp-path (string "janet-lsp-source-files-" (os/getpid))))
  (when (os/stat root) (remove-test-tree root))
  (os/mkdir root)
  (each directory ["ignored" ".hidden" "dist"]
    (os/mkdir (path/join root directory))
    (spit (path/join root directory "skip.janet") "(def skipped true)\n"))
  (spit (path/join root ".gitignore") "ignored/\n")
  (spit (path/join root ".hidden.janet") "(def hidden-file true)\n")
  (spit (path/join root "main.janet") "(def visible true)\n")
  (def git (os/spawn ["git" "init" "-q" root] :p))
  (test (os/proc-wait git) 0)
  (test (map path/basename (index/files root index/default-exclusions))
        @[".hidden.janet" "main.janet"])
  (remove-test-tree root))

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
                "(import ./value :only [shared] :prefix \"\" :export true)\n")
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

(deftest "enumerate public module exports and re-exports"
  (def base-uri "file:///workspace/base.janet")
  (def middle-uri "file:///workspace/middle.janet")
  (def workspace
    @{:index
      @{base-uri
        (index/analyze
          base-uri
          (string "(def public 1)\n"
                  "(def private :private 2)\n"
                  "(defn- hidden [] nil)\n"
                  "(def omitted 3)\n"))
        middle-uri
        (index/analyze
          middle-uri
          "(import ./base :only [public] :prefix \"\" :export true)\n")}})
  (test (map |($ :name) (index/exported-definitions workspace base-uri))
        @["public" "omitted"])
  (test (map |($ :name) (index/exported-definitions workspace middle-uri))
        @["public"])
  (test (index/exported-definition workspace base-uri "private" @{}) nil)
  (def imports
    (index/analyze
      "file:///workspace/imports.janet"
      (string "(import ./base :prefix \"m-\")\n"
              "(use ./base ./middle)\n"
              "(import ./base.janet)\n"
              "(import ./base :as chosen :prefix \"ignored-\" :export 1)\n")))
  (test (map |[($ :module) ($ :prefix)] (imports :imports))
        @[["./base" "m-"] ["./base" ""] ["./middle" ""]
          ["./base.janet" "base/"] ["./base" "chosen/"]])
  (test (get-in imports [:imports 4 :export]) true)

  (def before-uri "file:///workspace/before.janet")
  (def nested-uri "file:///workspace/nested.janet")
  (put (workspace :index) before-uri
       (index/analyze before-uri "public\n(import ./base :prefix \"\")\n"))
  (put (workspace :index) nested-uri
       (index/analyze nested-uri
                      "(defn load [] (import ./base :prefix \"\"))\npublic\n"))
  (test (index/resolve-definition workspace before-uri "public"
                                  {:line 0 :character 0})
        nil)
  (test (get-in (index/resolve-definition workspace before-uri "public"
                                           {:line 2 :character 0})
                [:name])
        "public")
  (test (index/resolve-definition workspace nested-uri "public"
                                  {:line 1 :character 0})
        nil)
  (test (index/resolve-definition workspace before-uri "base/public"
                                  {:line 0 :character 0})
        nil)
  (test (index/resolve-definition workspace nested-uri "base/public"
                                  {:line 1 :character 0})
        nil))

(deftest "persist and incrementally validate workspace index caches"
  (def root (platform/temp-path (string "janet-lsp-index-cache-" (os/getpid))))
  (def cache-root (string root "-data"))
  (def cache-parent (path/join cache-root "missing"))
  (def cache-directory (path/join cache-parent "nested"))
  (def source-path (path/join root "main.janet"))
  (def added-path (path/join root "added.janet"))
  (def cache-path (path/join cache-directory "index.jdn"))
  (when (os/stat cache-path) (os/rm cache-path))
  (when (os/stat cache-directory) (os/rmdir cache-directory))
  (when (os/stat cache-parent) (os/rmdir cache-parent))
  (when (os/stat cache-root) (os/rmdir cache-root))
  (when (os/stat added-path) (os/rm added-path))
  (when (os/stat source-path) (os/rm source-path))
  (when (os/stat root) (os/rmdir root))
  (os/mkdir root)
  (spit source-path "(def cached-value 1)\n")
  (def root-uri (uri/path->file-uri root))
  (def records (index/scan root index/default-exclusions))
  (test (index-cache/write cache-path root-uri index/default-exclusions records) true)
  (def loaded
    (index-cache/load cache-path root-uri root index/default-exclusions))
  (test (loaded :complete) true)
  (test (loaded :dirty) @[])
  (test (map |($ :name) (index/definitions {:index (loaded :index)}))
        @["cached-value"])

  (def document-uri (uri/path->file-uri source-path))
  (def forged (parse (slurp cache-path)))
  (def forged-entry (get-in forged [:entries document-uri]))
  (put (forged :entries) document-uri
       (merge forged-entry
              {:record (merge (forged-entry :record) {:content-hash "forged"})}))
  (spit cache-path (string/format "%j" forged))
  (def rejected
    (index-cache/load cache-path root-uri root index/default-exclusions))
  (test (rejected :complete) false)
  (test (deep= (rejected :dirty) @[source-path]) true)
  (index-cache/rebuild cache-path root-uri root index/default-exclusions)

  (test (not= (index/content-hash "(def v000Pay 1)")
              (index/content-hash "(def v000PbX 1)"))
        true)

  # Content hashes catch same-size rewrites even when metadata timestamps collide.
  (spit source-path "(def hacked-value 1)\n")
  (def changed
    (index-cache/load cache-path root-uri root index/default-exclusions))
  (test (changed :complete) false)
  (test (deep= (changed :dirty) @[source-path]) true)
  (def rebuilt
    (index-cache/rebuild cache-path root-uri root index/default-exclusions))
  (test (map |($ :name) (index/definitions {:index rebuilt})) @["hacked-value"])

  (spit added-path "(def added-value 1)\n")
  (def added (index-cache/load cache-path root-uri root index/default-exclusions))
  (test (added :complete) false)
  (test (deep= (added :dirty) @[added-path]) true)
  (index-cache/rebuild cache-path root-uri root index/default-exclusions)
  (os/rm added-path)
  (def removed (index-cache/load cache-path root-uri root index/default-exclusions))
  (test (removed :complete) false)
  (test (removed :dirty) @[])

  (spit cache-path "{:version 999 :entries {}}")
  (def incompatible
    (index-cache/load cache-path root-uri root index/default-exclusions))
  (test (incompatible :complete) false)
  (test (os/stat cache-path) nil)
  (os/rm source-path)
  (os/rmdir root)
  (os/rmdir cache-directory)
  (os/rmdir cache-parent)
  (os/rmdir cache-root))

(deftest "partition overlapping workspace indexes by owning root"
  (def parent-uri "file:///workspace")
  (def nested-uri "file:///workspace/nested")
  (def document-uri "file:///workspace/nested/main.janet")
  (def record (index/analyze document-uri "(def nested-value 1)\n"))
  (def parent @{:uri parent-uri :path "/workspace"
                :disk-index @{document-uri record} :index @{}})
  (def nested @{:uri nested-uri :path "/workspace/nested"
                :disk-index @{document-uri record} :index @{}})
  (def state @{:workspaces @{parent-uri parent nested-uri nested}
               :standalone-workspace @{:path nil :index @{}}})
  (workspace/partition-indexes state)
  (test (get (parent :index) document-uri) nil)
  (test (not (nil? (get (nested :index) document-uri))) true)
  (put (state :workspaces) nested-uri nil)
  (workspace/partition-indexes state)
  (test (not (nil? (get (parent :index) document-uri))) true))

(deftest "replay watcher changes over completed workspace scans"
  (def changed-uri "file:///workspace/changed.janet")
  (def deleted-uri "file:///workspace/deleted.janet")
  (def scanned
    @{changed-uri (index/analyze changed-uri "(def stale 1)\n")
      deleted-uri (index/analyze deleted-uri "(def deleted 1)\n")})
  (def current (index/analyze changed-uri "(def current 2)\n"))
  (workspace/merge-scan-changes
    scanned @{changed-uri current deleted-uri :deleted})
  (test (map |($ :name) (index/definitions {:index scanned})) @["current"])
  (test (get scanned deleted-uri) nil))

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

(deftest "report conservative parser-backed diagnostics"
  (def source
    (string "(def duplicate 1)\n"
            "(def duplicate 2)\n"
            "(defn outer [value]\n"
            "  (fn [value] value)\n"
            "  (if false 1 2)\n"
            "  (break)\n"
            "  missing)\n"
            "(quote quoted-missing)\n"))
  (def tree (parser/syntax-tree source))
  (def record (index/analyze "file:///safe.janet" source tree))
  (def found
    (static-diagnostics/analyze source tree record @{:index @{}} root-env))
  (def counts (frequencies (map |($ :code) found)))
  (test (get counts "janet.lint.undefined-symbol") 1)
  (test (get counts "janet.lint.duplicate-definition") 1)
  (test (get counts "janet.lint.shadowing") 1)
  (test (get counts "janet.lint.constant-condition") 1)
  (test (get counts "janet.lint.unreachable-code") 2)
  (test (get-in (first (filter |(= "janet.lint.undefined-symbol" ($ :code)) found))
                [:message])
        "undefined symbol missing")
  (def vector-source "(def data [not-a-binding])\n")
  (def vector-tree (parser/syntax-tree vector-source))
  (def vector-record (index/analyze "file:///vector.janet" vector-source vector-tree))
  (test (map |($ :message)
             (static-diagnostics/analyze vector-source vector-tree vector-record
                                         @{:index @{}} root-env))
        @["undefined symbol not-a-binding"])
  (def opaque-source
    "(unknown-macro argument-data (if false branch-a branch-b))\n")
  (def opaque-tree (parser/syntax-tree opaque-source))
  (def opaque-record (index/analyze "file:///opaque.janet" opaque-source opaque-tree))
  (test (map |($ :message)
             (static-diagnostics/analyze opaque-source opaque-tree opaque-record
                                         @{:index @{}} root-env))
        @["undefined symbol unknown-macro"])
  (def quoted-source "(quote (if false missing-a missing-b))\n")
  (def quoted-tree (parser/syntax-tree quoted-source))
  (def quoted-record (index/analyze "file:///quoted.janet" quoted-source quoted-tree))
  (test (static-diagnostics/analyze quoted-source quoted-tree quoted-record
                                    @{:index @{}} root-env)
        @[])
  (def forward-source
    "(future-call opaque-argument)\n(defn future-call [value] value)\n(def first later)\n(def later 1)\n")
  (def forward-tree (parser/syntax-tree forward-source))
  (def forward-record
    (index/analyze "file:///forward.janet" forward-source forward-tree))
  (test (map |($ :message)
             (static-diagnostics/analyze forward-source forward-tree forward-record
                                         @{:index @{}} root-env))
        @["undefined symbol future-call" "undefined symbol later"])
  (def sequential-source "(let [f (fn [x] x) x 1] (f x))\n")
  (def sequential-tree (parser/syntax-tree sequential-source))
  (def sequential-record
    (index/analyze "file:///sequential.janet" sequential-source sequential-tree))
  (test (map |($ :message)
             (static-diagnostics/analyze sequential-source sequential-tree
                                         sequential-record @{:index @{}} root-env))
        @[])
  (def earlier-source "(let [x 1 y (fn [x] x)] y)\n")
  (def earlier-tree (parser/syntax-tree earlier-source))
  (def earlier-record
    (index/analyze "file:///earlier.janet" earlier-source earlier-tree))
  (test (map |($ :message)
             (static-diagnostics/analyze earlier-source earlier-tree earlier-record
                                         @{:index @{}} root-env))
        @["unused binding x" "binding x shadows an existing binding"])
  (def destructured-source "(let [[a b] [1 2] c (fn [a] a)] c)\n")
  (def destructured-tree (parser/syntax-tree destructured-source))
  (def destructured-record
    (index/analyze "file:///destructured.janet" destructured-source
                   destructured-tree))
  (test (map |($ :message)
             (static-diagnostics/analyze
               destructured-source destructured-tree destructured-record
               @{:index @{}} root-env))
        @["unused binding a" "unused binding b"
          "binding a shadows an existing binding"])
  (def loop-source "(defn run [xs] (loop [x :in xs] x))\n")
  (def loop-tree (parser/syntax-tree loop-source))
  (def loop-record (index/analyze "file:///loop.janet" loop-source loop-tree))
  (test (static-diagnostics/analyze loop-source loop-tree loop-record
                                    @{:index @{}} root-env)
        @[]))

(deftest "resolve imports and suppress configured diagnostic categories"
  (def module-uri "file:///workspace/module.janet")
  (def main-uri "file:///workspace/main.janet")
  (def module-record (index/analyze module-uri "(def exported 1)\n"))
  (def source
    (string "(import ./module :as module)\n"
            "(module/exported)\n"
            "(module/missing)\n"
            "# janet-lsp: ignore-next-line undefinedSymbol\n"
            "ignored-name\n"
            "# janet-lsp: disable constantCondition\n"
            "(if true 1 2)\n"))
  (def tree (parser/syntax-tree source))
  (def record (index/analyze main-uri source tree))
  (def raw
    (static-diagnostics/analyze
      source tree record @{:index @{module-uri module-record}} root-env))
  (def visible (configuration/suppress raw source))
  (test (map |($ :message)
             (filter |(= "janet.lint.undefined-symbol" ($ :code)) visible))
        @["undefined symbol module/missing"])
  (test (has-value? (map |($ :code) visible) "janet.lint.constant-condition") false)
  (def settings
    (configuration/diagnostics
      {"diagnostics" {"unreachableCode" "hint"
                      "duplicateDefinition" "off"}}))
  (test (get settings "unreachableCode") 4)
  (test (get settings "duplicateDefinition") false)
  (def configured
    (configuration/apply-severity
      {:code "janet.lint.unreachable-code" :severity 2} settings))
  (test (configured :code) "janet.lint.unreachable-code")
  (test (configured :severity) 4)
  (test (configuration/apply-severity
          {:code "janet.lint.duplicate-definition" :severity 1} settings)
        nil)
  (def string-directive-source
    "(def text `# janet-lsp: disable undefinedSymbol`)\nstill-missing\n")
  (test (map |($ :message)
             (configuration/suppress
               [{:code "janet.lint.undefined-symbol"
                 :message "undefined symbol still-missing"
                 :location [2 1]}]
               string-directive-source))
        @["undefined symbol still-missing"])
  (def external-source
    "(import spork/json :as json)\n(json/decode \"{}\")\nlocal-missing\n")
  (def external-tree (parser/syntax-tree external-source))
  (def external-record
    (index/analyze "file:///external.janet" external-source external-tree))
  (test (map |($ :message)
             (static-diagnostics/analyze
               external-source external-tree external-record
               @{:index @{} :cache-current true :path nil} root-env))
        @["undefined symbol local-missing"]))

(deftest "index callable identities and executable call sites"
  (def document-uri "file:///calls.janet")
  (def source
    (string "(defn target [x] (target x))\n"
            "(defmacro expand [x] x)\n"
            "(defn caller [x]\n"
            "  (target x)\n"
            "  (expand x)\n"
            "  (let [local (fn [y] (target y))]\n"
            "    (local x))\n"
            "  '(target x)\n"
            "  ~(do ,(target x)))\n"))
  (def record (index/analyze document-uri source))
  (def workspace @{:index @{document-uri record}})
  (index/relink workspace)
  (test (map |[($ :name) ($ :local)] (record :callables))
        @[["target" false] ["expand" false] ["caller" false] ["local" true]])
  (def caller (first (filter |(= "caller" ($ :name)) (record :callables))))
  (def local (first (filter |(= "local" ($ :name)) (record :callables))))
  (test (map |($ :name) (index/outgoing-calls workspace (caller :identity)))
        @["target" "expand" "local" "target"])
  (test (map |($ :name) (index/outgoing-calls workspace (local :identity)))
        @["target"])
  (test (length (index/incoming-calls
                  workspace
                  (get-in record [:callables 0 :identity])))
        4)

  (def duplicate-uri "file:///duplicates.janet")
  (def duplicate-record
    (index/analyze duplicate-uri
                   "(defn same [] nil)\n(defn same [] nil)\n(defn invoke [] (same))\n"))
  (def duplicate-workspace @{:index @{duplicate-uri duplicate-record}})
  (index/relink duplicate-workspace)
  (def invoke
    (first (filter |(= "invoke" ($ :name)) (duplicate-record :callables))))
  (test (index/outgoing-calls duplicate-workspace (invoke :identity)) @[])

  (def nested-uri "file:///nested-calls.janet")
  (def nested-record
    (index/analyze
      nested-uri
      (string "(defn target [] nil)\n"
              "(defn outer []\n"
              "  (let [a (fn [] (let [b (fn [] (target))] (b)))] (a))\n"
              "  (if-let [conditional (fn [] (target))] (conditional)))\n")))
  (def nested-workspace @{:index @{nested-uri nested-record}})
  (index/relink nested-workspace)
  (def nested-by-name
    (fn [name] (filter |(= name ($ :name)) (nested-record :callables))))
  (test (length (nested-by-name "b")) 1)
  (test (length (nested-by-name "conditional")) 1)
  (test (map |($ :name)
             (index/outgoing-calls nested-workspace
                                   (get-in (nested-by-name "a") [0 :identity])))
        @["b"])
  (test (map |($ :name)
             (index/outgoing-calls
               nested-workspace
               (get-in (nested-by-name "conditional") [0 :identity])))
        @["target"])

  (def named-record
    (index/analyze
      "file:///nested-named.janet"
      (string "(defn left [] (defn inner [] nil) (inner))\n"
              "(defn right [] (defn inner [] nil) (inner))\n")))
  (def named-workspace @{:index @{(named-record :uri) named-record}})
  (index/relink named-workspace)
  (each name ["left" "right"]
    (def callable (first (filter |(= name ($ :name)) (named-record :callables))))
    (test (map |($ :name)
               (index/outgoing-calls named-workspace (callable :identity)))
          @["inner"])))

(deftest "reindex calls after editing a function"
  (def document-uri "file:///edited-function.janet")
  (def workspace @{:index @{}})
  (index/update workspace document-uri
                (string "(defn before [] nil)\n"
                        "(defn after [] nil)\n"
                        "(defn caller [] (before))\n"))
  (def callable
    (first (filter |(= "caller" ($ :name)) (index/callables workspace))))
  (test (map |($ :name) (index/outgoing-calls workspace (callable :identity)))
        @["before"])

  (index/update workspace document-uri
                (string "(defn before [] nil)\n"
                        "(defn after [] nil)\n"
                        "(defn caller [] (after))\n"))
  (def updated
    (first (filter |(= "caller" ($ :name)) (index/callables workspace))))
  (test (map |($ :name) (index/outgoing-calls workspace (updated :identity)))
        @["after"])
  (def before
    (first (filter |(= "before" ($ :name)) (index/callables workspace))))
  (test (index/incoming-calls workspace (before :identity)) @[]))

(deftest "resolve explicit type and implementation metadata"
  (def document-uri "file:///navigation-metadata.janet")
  (def record
    (index/analyze
      document-uri
      (string "(def Shape {})\n"
              "(def Circle {:janet-lsp/type-definition \"Shape\"} 1)\n"
              "(defn draw-circle {:janet-lsp/implements \"Shape\"} [circle] circle)\n"
              "(defn draw-square {:janet-lsp/implements [\"Shape\" \"Drawable\"]} [square] square)\n"
              "(def inferred {})\n"
              "(def malformed {:janet-lsp/type-definition 42} nil)\n")))
  (def workspace @{:index @{document-uri record}})
  (index/relink workspace)
  (def by-name
    (fn [name] (first (filter |(= name ($ :name)) (record :definitions)))))
  (test (get-in (by-name "Circle") [:type-target :name]) "Shape")
  (test (map |($ :name) (get-in (by-name "draw-square")
                                [:implementation-targets]))
        @["Shape" "Drawable"])
  (test (get-in (index/type-definition workspace ((by-name "Circle") :identity))
                [:name])
        "Shape")
  (test (index/type-definition workspace ((by-name "inferred") :identity)) nil)
  (test (index/type-definition workspace ((by-name "malformed") :identity)) nil)
  (test (map |($ :name) (index/implementations workspace ((by-name "Shape") :identity)))
        @["draw-circle" "draw-square"])

  (def duplicate-record
    (index/analyze "file:///ambiguous-metadata.janet"
                   (string "(def Same {})\n"
                           "(def Same {})\n"
                           "(def value {:janet-lsp/type-definition \"Same\"} {})\n")))
  (def duplicate-workspace
    @{:index @{(duplicate-record :uri) duplicate-record}})
  (index/relink duplicate-workspace)
  (def value (first (filter |(= "value" ($ :name))
                            (duplicate-record :definitions))))
  (test (index/type-definition duplicate-workspace (value :identity)) nil)

  (def imported-type-uri "file:///types.janet")
  (def imported-main-uri "file:///main.janet")
  (def imported-type-record
    (index/analyze imported-type-uri "(def Kind {})\n(def Kind {})\n"))
  (def imported-main-record
    (index/analyze
      imported-main-uri
      (string "(import ./types :as types)\n"
              "(def value {:janet-lsp/type-definition \"types/Kind\"} {})\n")))
  (def imported-workspace
    @{:index @{imported-type-uri imported-type-record
               imported-main-uri imported-main-record}})
  (index/relink imported-workspace)
  (def imported-value
    (first (filter |(= "value" ($ :name))
                   (imported-main-record :definitions))))
  (test (index/type-definition imported-workspace (imported-value :identity)) nil))

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
  (test (map |($ :code)
             (signatures/diagnostics
               source (index/analyze "file:///workspace/main.janet" source)))
        @["janet.call.missing-arguments"
          "janet.call.extra-arguments"
          "janet.call.unknown-named-argument"
          "janet.call.duplicate-named-argument"
          "janet.call.odd-named-arguments"])
  (def quoted-unquote
    (string "(defn target [x] nil)\n"
            "(quasiquote (unquote (target)))\n"
            "(defn caller [target] (target))\n"))
  (test (signatures/diagnostics
          quoted-unquote
          (index/analyze "file:///workspace/quoted.janet" quoted-unquote))
        @[])
  (def missing
    (first (filter |(= "janet.call.missing-arguments" ($ :code))
                   (signatures/diagnostics source))))
  (test (missing :data) @{:callee "exact" :missing 1 :provided 0})
  (def unknown
    (first (filter |(= "janet.call.unknown-named-argument" ($ :code))
                   (signatures/diagnostics source))))
  (test (unknown :data)
        @{:callee "run" :label ":unknown" :positional 2 :named-index 0})
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

(deftest "derive portable filesystem and process conventions"
  (test (platform/path-list-separator :windows) ";")
  (test (platform/path-list-separator :linux) ":")
  (test (platform/normalize-path "C:/Users/NAME" :windows) "c:/users/name")
  (test (platform/normalize-path "/Users/Name" :macos) "/Users/Name")
  (test (platform/executable-names "janet" :windows [".EXE" ".CMD"])
        @["janet.exe" "janet.cmd"])
  (test (deep= (platform/executable-names "janet" :macos) ["janet"]) true)
  (test (string? (platform/temp-directory)) true))

(deftest "reject non-file and malformed URIs"
  (test (uri/file-uri->path "untitled:buffer") nil)
  (test (uri/file-uri->path "file:///tmp/a#fragment") nil)
  (test-error (uri/file-uri->path "file:///tmp/%GG")
              "invalid percent escape in URI"))

(deftest "untrusted analysis does not execute workspace code"
  (def prefix (platform/temp-path (string "janet-lsp-safe-analysis-" (os/getpid))))
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
