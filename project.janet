(declare-project
  :name "janet-lsp"
  :description "A Language Server (LSP) for the Janet Programming Language"
  :version "0.0.12"
  :dependencies ["https://github.com/janet-lang/spork.git"
                 {:url "https://github.com/ianthehenry/cmd.git"
                  :tag "v1.1.0"}
                 {:url "https://github.com/ianthehenry/judge.git"
                  :tag "v2.9.0"}])

(task "test" []
      (shell "jpm_tree/bin/judge" "test/test-main.janet"
             "test/test-lookup.janet"
             "test/test-parser.janet"
             "test/test-integration.janet"))

(declare-archive
  :name "janet-lsp"
  :entry "/src/main")

(declare-binscript
  :main "src/janet-lsp"
  :hardcode-syspath true
  :is-janet true)
