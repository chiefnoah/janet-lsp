(import spork/path)
(import ./lookup)
(import ./uri)

(def default-exclusions [".git" ".hg" ".svn" "build" "dist" "node_modules" "jpm_tree"])
(def definition-heads ["def" "def-" "defn" "defn-" "defmacro" "defmacro-" "var" "var-"])

(defn analyze [document-uri content]
  (def definitions @[])
  (def references @[])
  (eachp [line-number line] (string/split "\n" (lookup/code-mask content))
    (def tokens (filter |(not (empty? ($ 1))) (or (peg/match lookup/word-peg line) @[])))
    (each token tokens
      (array/push references {:name (token 1) :uri document-uri
                              :range {:start {:line line-number :character (token 0)}
                                      :end {:line line-number :character (token 2)}}}))
    (when (and (>= (length tokens) 2) (has-value? definition-heads ((tokens 0) 1)))
      (def token (tokens 1))
      (array/push definitions {:name (token 1) :uri document-uri
                               :kind (if (string/has-prefix? "defn" ((tokens 0) 1)) 12 13)
                               :range {:start {:line line-number :character (token 0)}
                                       :end {:line line-number :character (token 2)}}})))
  {:uri document-uri :definitions definitions :references references})

(defn update [workspace document-uri content]
  (put (workspace :index) document-uri (analyze document-uri content)))

(defn remove [workspace document-uri]
  (put (workspace :index) document-uri nil))

(defn definitions [workspace &opt name]
  (catseq [record :in (values (workspace :index))
           definition :in (record :definitions)
           :when (or (nil? name) (= name (definition :name)))] definition))

(defn references [workspace name]
  (catseq [record :in (values (workspace :index))
           reference :in (record :references)
           :when (= name (reference :name))] reference))

(defn scan [root exclusions]
  (def records @{})
  (def pending @[root])
  (while (not (empty? pending))
    (def current (array/pop pending))
    (case (os/stat current :mode)
      :directory (unless (has-value? exclusions (path/basename current))
                   (each entry (os/dir current) (array/push pending (path/join current entry))))
      :file (when (and (string/has-suffix? ".janet" current)
                       (not (any? (map |(has-value? exclusions $)
                                       (string/split "/" current)))))
              (try (put records (uri/path->file-uri current) (analyze (uri/path->file-uri current) (slurp current)))
                ([_] nil)))))
  records)
