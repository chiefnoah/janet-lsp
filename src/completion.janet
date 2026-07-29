(import ./index)
(import ./lookup)
(import ./parser)
(import ./server-utils)
(import ./signatures)
(import ./utils)
(import ./uri)
(import spork/path)

(def module-heads {"import" true "use" true "require" true "dofile" true})
(def definition-heads
  {"def" true "def-" true "var" true "var-" true
   "defn" true "defn-" true "defmacro" true "defmacro-" true
   "varfn" true "varfn-" true})
(def function-heads
  {"fn" true "defn" true "defn-" true "defmacro" true
   "defmacro-" true "varfn" true "varfn-" true})

(def standard-keywords
  [":private" ":deprecated" ":as" ":prefix" ":export" ":exit"
   ":fresh" ":only" ":janet-lsp/type-definition" ":janet-lsp/implements"])

(def snippets
  [{:label "def" :detail "Define an immutable binding"
    :body "(def ${1:name} ${2:value})"}
   {:label "defn" :detail "Define a function"
    :body "(defn ${1:name} [${2:args}]\n  ${0:body})"}
   {:label "fn" :detail "Create a function"
    :body "(fn [${1:args}]\n  ${0:body})"}
   {:label "let" :detail "Bind lexical values"
    :body "(let [${1:name} ${2:value}]\n  ${0:body})"}
   {:label "if" :detail "Branch on a condition"
    :body "(if ${1:condition}\n  ${2:then}\n  ${0:else})"}
   {:label "when" :detail "Evaluate when a condition is truthy"
    :body "(when ${1:condition}\n  ${0:body})"}
   {:label "each" :detail "Iterate over a sequence"
    :body "(each ${1:item} ${2:sequence}\n  ${0:body})"}])

(defn binding-item [name eval-env]
  (def value (get-in eval-env [name :value] name))
  {:label name
   :kind (case (type value)
           :symbol 12 :boolean 6
           :function 3 :cfunction 3
           :string 6 :buffer 6
           :number 6 :keyword 6
           :core/file 17 :core/peg 6
           :struct 6 :table 6
           :tuple 6 :array 6
           :fiber 6 :nil 6
           6)})

(defn- separator? [byte]
  (or (has-value? [0 9 10 11 12 13 32 34 39 40 41 44 59 91 93 96 123 125]
                  byte)
      (= byte 35)))

(defn- token-at [location source]
  (def cursor (min (lookup/to-index location source) (length source)))
  (def bytes (string/bytes source))
  (var start cursor)
  (var end cursor)
  (while (and (> start 0) (not (separator? (bytes (dec start)))))
    (-= start 1))
  (while (and (< end (length bytes)) (not (separator? (bytes end))))
    (+= end 1))
  {:prefix (string/slice source start cursor)
   :word (string/slice source start end)
   :start start
   :end end
   :range {:start (lookup/from-index start source)
           :end (lookup/from-index end source)}})

(defn- source-mode [source cursor]
  (def bytes (string/bytes source))
  (var state :code)
  (var escaped false)
  (var delimiter 0)
  (var index 0)
  (while (< index (min cursor (length bytes)))
    (def byte (bytes index))
    (case state
      :comment
      (when (= byte 10) (set state :code))

      :string
      (cond
        escaped (set escaped false)
        (= byte 92) (set escaped true)
        (= byte 34) (set state :code))

      :long-string
      (when (= byte 96)
        (var count 0)
        (while (and (< (+ index count) (length bytes))
                    (= 96 (bytes (+ index count))))
          (+= count 1))
        (def consumed (if (>= count delimiter) delimiter count))
        (when (>= count delimiter) (set state :code))
        (+= index (dec consumed)))

      (cond
        (= byte 35) (set state :comment)
        (= byte 34) (set state :string)
        (= byte 96)
        (do
          (var count 0)
          (while (and (< (+ index count) (length bytes))
                      (= 96 (bytes (+ index count))))
            (+= count 1))
          (set delimiter count)
          (set state :long-string)
          (+= index (dec count)))))
    (+= index 1))
  state)

(defn- structural-stack [source cursor]
  (def bytes (string/bytes (lookup/structure-mask source)))
  (def stack @[])
  (for index 0 (min cursor (length bytes))
    (case (bytes index)
      40 (array/push stack {:delimiter 40 :index index})
      91 (array/push stack {:delimiter 91 :index index})
      123 (array/push stack {:delimiter 123 :index index})
      41 (when (and (not (empty? stack)) (= 40 ((last stack) :delimiter)))
           (array/pop stack))
      93 (when (and (not (empty? stack)) (= 91 ((last stack) :delimiter)))
           (array/pop stack))
      125 (when (and (not (empty? stack)) (= 123 ((last stack) :delimiter)))
            (array/pop stack))))
  stack)

(defn- direct-state [source start end]
  (def bytes (string/bytes (lookup/structure-mask source)))
  (var depth 0)
  (var in-form false)
  (var forms 0)
  (for index (inc start) (min end (length bytes))
    (def byte (bytes index))
    (def whitespace? (has-value? [0 9 10 11 12 13 32] byte))
    (when (and (= depth 0) (not whitespace?) (not in-form))
      (set in-form true)
      (+= forms 1))
    (when (and (= depth 0) whitespace? in-form)
      (set in-form false))
    (cond
      (has-value? [40 91 123] byte) (+= depth 1)
      (has-value? [41 93 125] byte) (when (> depth 0) (-= depth 1))))
  {:forms forms :in-form in-form
   :previous (- forms (if in-form 1 0))})

(defn- call-head [source start]
  (def bytes (string/bytes (lookup/structure-mask source)))
  (var index (inc start))
  (while (and (< index (length bytes))
              (has-value? [0 9 10 11 12 13 32] (bytes index)))
    (+= index 1))
  (def head-start index)
  (while (and (< index (length bytes)) (not (separator? (bytes index))))
    (+= index 1))
  (string/slice source head-start index))

(defn- first-direct-bracket [source start end]
  (def bytes (string/bytes (lookup/structure-mask source)))
  (var depth 0)
  (var found nil)
  (for index (inc start) (min end (length bytes))
    (def byte (bytes index))
    (when (and (= depth 0) (= byte 91) (nil? found))
      (set found index))
    (cond
      (has-value? [40 91 123] byte) (+= depth 1)
      (has-value? [41 93 125] byte) (when (> depth 0) (-= depth 1))))
  found)

(defn- binding-context [source cursor call stack]
  (def callee (and call (call :callee)))
  (or
    (when (and callee
               (= 0 (call :active-parameter))
               (has-value? ["def" "def-" "var" "var-" "defn" "defn-"
                            "defmacro" "defmacro-" "varfn" "varfn-"
                            "each" "for"]
                           callee))
      :name)
    (some
      (fn [entry-index]
        (def entry (stack entry-index))
        (when (= 91 (entry :delimiter))
          (def parent (and (> entry-index 0) (stack (dec entry-index))))
          (when (and parent (= 40 (parent :delimiter)))
            (let [head (call-head source (parent :index))
                  position (direct-state source (parent :index) (inc (entry :index)))
                  argument (- (position :forms) 2)]
              (cond
                (and (get function-heads head)
                     (= (entry :index)
                        (first-direct-bracket source (parent :index)
                                              (inc (entry :index))))) :parameters
                (and (= "let" head) (= argument 0)
                     (even? (get-in (direct-state source (entry :index) cursor)
                                    [:previous]))) :binding
                (and (= "loop" head) (= argument 0)
                     (zero? (% (get-in (direct-state source (entry :index) cursor)
                                        [:previous])
                               3))) :binding
                nil)))))
      (reverse (range 0 (length stack))))
    (and call (= "let" callee) (= 0 (call :active-parameter))
         (not (any? (map |(= 91 ($ :delimiter)) stack)))
         :binding)))

(defn- table-key-context? [source cursor stack]
  (when-let [entry (last (filter |(= 123 ($ :delimiter)) stack))]
    (even? (get-in (direct-state source (entry :index) cursor) [:previous]))))

(defn- table-key-call-context? [call]
  (and call
       (= 1 (call :active-parameter))
       (has-value? ["get" "in" "put" "has-key?"] (call :callee))))

(defn- metadata-context? [source token call prefix]
  (and call (get definition-heads (call :callee))
       (> (call :active-parameter) 0)
       (string/has-prefix? ":" prefix)
       (or (not (get function-heads (call :callee)))
           (let [call-start (dec (first (call :range)))
                 parameters (first-direct-bracket source call-start (token :start))]
             (or (nil? parameters) (< (token :start) parameters))))))

(defn- module-context? [call prefix]
  (and call (get module-heads (call :callee))
       (not (string/has-prefix? ":" prefix))
       (or (= "use" (call :callee))
           (= 0 (call :active-parameter)))))

(defn- public-definition? [definition]
  (and (definition :top-level)
       (not (definition :private))
       (not (string/has-suffix? "-" (definition :form)))))

(defn- source-module-path [document-path target-path dofile?]
  (when (and document-path target-path (not= document-path target-path))
    (var relative
      (string/replace-all "\\" "/"
                          (path/relpath (path/dirname document-path) target-path)))
    (unless dofile?
      (when (string/has-suffix? "/init.janet" relative)
        (set relative (path/dirname relative)))
      (each extension [".janet" ".jimage" ".so"]
        (when (string/has-suffix? extension relative)
          (set relative (string/slice relative 0 (- (length relative)
                                                    (length extension)))))))
    (if (or (string/has-prefix? "." relative)
            (string/has-prefix? "/" relative))
      relative
      (string "./" relative))))

(defn- module-candidates [workspace document dofile?]
  (distinct
    (catseq [document-uri :in (keys (workspace :index))
             :let [target-path (uri/file-uri->path document-uri)
                   module (source-module-path (document :path) target-path dofile?)]
             :when module]
      module)))

(defn- usage-counts [workspace]
  (def counts @{})
  (each record (values (workspace :index))
    (each reference (record :references)
      (def name (reference :name))
      (put counts name (inc (get counts name 0)))))
  counts)

(defn- rank [tier usage label]
  (string/format "%d:%08d:%s" tier (- 999999 (min 999999 usage)) label))

(defn- lsp-token-edit [state content token new-text]
  {:range (server-utils/lsp-range state content (token :range))
   :newText new-text})

(defn- module-expression [module]
  (if (any? (map separator? (string/bytes module)))
    (string/format "%q" module)
    module))

(defn- quoted-token? [content token]
  (and (> (token :start) 0)
       (has-value? [34 96] ((string/bytes content) (dec (token :start))))))

(defn- module-items [state workspace document token call usage]
  (def dofile? (= "dofile" (call :callee)))
  (def content (document :content))
  (catseq [module :in (module-candidates workspace document dofile?)
           :when (string/has-prefix? (token :prefix) module)]
    {:label module
     :kind 9
     :detail (if dofile? "Workspace source file" "Workspace module")
     :sortText (rank 0 (get usage module 0) module)
     :textEdit (lsp-token-edit state content token
                               (if (quoted-token? content token)
                                 module
                                 (module-expression module)))}))

(defn- visible-imports [document cursor]
  (def content (document :content))
  (filter |(and ($ :top-level)
                (<= (lookup/to-index (get-in $ [:range :end]) content) cursor))
          (get-in document [:analysis :index :imports] @[])))

(defn- imported-items [workspace document imports usage]
  (def found @[])
  (each imported imports
    (when (has-value? ["import" "import*" "use"] (imported :kind))
      (when-let [target-uri
                 (index/module-uri workspace (document :uri) (imported :module))]
        (each definition (index/exported-definitions workspace target-uri)
          (when (or (empty? (imported :only))
                    (has-value? (imported :only) (definition :name)))
            (def label (string (imported :prefix) (definition :name)))
            (array/push found
                        {:label label
                         :kind (definition :kind)
                         :detail (string "Imported from " (imported :module))
                         :sortText (rank 1 (get usage label 0) label)}))))))
  (utils/concat-dedup-by-label found))

(defn- call-module [content call]
  (def fragment (and call (string/slice content (first (call :range)))))
  (def words
    (and fragment
         (map |($ 1)
              (filter |(not (empty? ($ 1)))
                      (or (peg/match lookup/word-peg fragment) @[])))))
  (and (> (length words) 1) (words 1)))

(defn- only-items [state workspace document token call stack usage]
  (def bracket (last (filter |(= 91 ($ :delimiter)) stack)))
  (def content (document :content))
  (def call-source
    (and call (string/slice content (first (call :range))
                            (token :start))))
  (if (and bracket call (= "import" (call :callee)) call-source
           (string/find ":only" call-source))
    (when-let [module (call-module content call)
               target-uri
               (index/module-uri workspace (document :uri) module)]
      (catseq [definition :in (index/exported-definitions workspace target-uri)
               :let [name (definition :name)]
               :when (string/has-prefix? (token :prefix) name)]
        {:label name :kind (definition :kind)
         :detail (string "Exported by " module)
         :sortText (rank 0 (get usage name 0) name)
         :textEdit (lsp-token-edit state (document :content) token name)}))
    nil))

(defn- keyword-items [workspace prefix usage &opt metadata?]
  (def found (array ;standard-keywords))
  (each record (values (workspace :index))
    (each reference (record :references)
      (when (string/has-prefix? ":" (reference :name))
        (array/push found (reference :name)))))
  (catseq [keyword :in (sort (distinct found))
           :when (string/has-prefix? prefix keyword)]
    {:label keyword :kind 14
     :detail (if metadata? "Binding metadata" "Keyword")
     :sortText (rank 0 (get usage keyword 0) keyword)}))

(defn- binding-items [context prefix]
  (if (= :parameters context)
    (catseq [marker :in ["&" "&opt" "&named"]
             :when (string/has-prefix? prefix marker)]
      {:label marker :kind 14 :detail "Parameter binding marker"
       :sortText (string "0:" marker)})
    @[]))

(defn- import-position [record content]
  (if-let [imported (last (sort-by |(get-in $ [:range :end :line])
                                  (filter |($ :top-level) (record :imports))))]
    (get-in imported [:range :end])
    (if (string/has-prefix? "#!" content)
      (if (string/find "\n" content)
        {:line 1 :character 0}
        {:line 0 :character (length content)})
      {:line 0 :character 0})))

(defn- import-edit [state document module name]
  (def position (import-position (get-in document [:analysis :index])
                                 (document :content)))
  (def after-import?
    (any? (map |($ :top-level) (get-in document [:analysis :index :imports] @[]))))
  (def after-shebang? (and (not after-import?)
                            (string/has-prefix? "#!" (document :content))))
  (def shebang-line-complete?
    (and after-shebang? (string/find "\n" (document :content))))
  (def newline (if (string/find "\r\n" (document :content)) "\r\n" "\n"))
  {:range (server-utils/lsp-range state (document :content)
                                   {:start position :end position})
    :newText (string (if (or after-import?
                             (and after-shebang? (not shebang-line-complete?)))
                       newline "")
                     "(import " (module-expression module) " :only [" name
                     "] :prefix \"\")" newline)})

(defn missing-import-edit [state document name]
  (def workspace (server-utils/document-workspace state document))
  (def candidates
    (catseq [definition :in (index/definitions workspace name)
             :when (and (public-definition? definition)
                        (not= (document :uri) (definition :uri)))
             :let [module
                   (source-module-path (document :path)
                                       (uri/file-uri->path (definition :uri)) false)]
             :when module]
      {:definition definition :module module}))
  (def last-import
    (last (sort-by |(get-in $ [:range :end :line])
                   (filter |($ :top-level)
                           (get-in document [:analysis :index :imports] @[])))))
  (def safe-insertion
    (or (nil? last-import)
        (let [line (get-in last-import [:range :end :line])
              character (get-in last-import [:range :end :character])
              text (get (string/split "\n" (document :content)) line)]
          (empty? (string/trim (string/slice text character))))))
  (when (and (workspace :uri) (workspace :cache-current) (nil? (workspace :scan))
             safe-insertion (= 1 (length candidates)))
    (import-edit state document (get-in candidates [0 :module]) name)))

(defn- auto-import-items [state workspace document token prefix visible usage]
  (if (empty? prefix)
    @[]
    (let [candidates @{}
          document-uri (document :uri)]
      (each definition (index/definitions workspace)
        (def name (definition :name))
        (when (and (public-definition? definition)
                   (not= document-uri (definition :uri))
                   (not (get visible name))
                   (string/has-prefix? prefix name))
          (when-let [module
                     (source-module-path (document :path)
                                         (uri/file-uri->path (definition :uri)) false)]
            (def previous (get candidates name))
            (cond
              (nil? previous)
              (put candidates name {:definition definition :module module})

              (not= (get-in previous [:definition :uri]) (definition :uri))
              (put candidates name false)))))
      (seq [[name candidate] :pairs candidates
            :when candidate]
        {:label name
         :kind (get-in candidate [:definition :kind])
         :detail (string "Auto import from " (candidate :module))
         :sortText (rank 3 (get usage name 0) name)
         :textEdit (lsp-token-edit state (document :content) token name)
         :additionalTextEdits
         [(import-edit state document (candidate :module) name)]}))))

(defn- snippet-items [content token prefix call bare-call?]
  (catseq [snippet :in snippets
           :when (string/has-prefix? prefix (snippet :label))]
    (let [inside-call? (or bare-call? (and call (= prefix (call :callee))))
          has-closing? (and inside-call? (< (token :end) (length content))
                            (= 41 ((string/bytes content) (token :end))))
          without-opening (if inside-call?
                            (string/slice (snippet :body) 1)
                            (snippet :body))]
      {:label (snippet :label)
       :kind 15
       :detail (snippet :detail)
       :insertText (if has-closing?
                     (string/slice without-opening 0 (dec (length without-opening)))
                     without-opening)
       :insertTextFormat 2
       :sortText (string "4:" (snippet :label))})))

(defn- visible-set [items]
  (def found @{})
  (each item items (put found (string (item :label)) true))
  found)

(defn complete [state document location]
  (let [content (document :content)
        cursor (lookup/to-index location content)
        token (token-at location content)
        prefix (token :prefix)
        mode (source-mode content cursor)
        stack (structural-stack content cursor)
        call (lookup/call-context location content)
        workspace (server-utils/document-workspace state document)
        usage (usage-counts workspace)
        signature (and call
                       (signatures/find-in (get-in document [:analysis :signatures] @[])
                                           (call :callee)))
        used-named (if signature
                     (signatures/used-named-arguments content call signature)
                     @[])
        named-items
        (if (and signature
                 (>= (call :active-parameter)
                     (max 0 (dec (signature :positional)))))
          (catseq [parameter :in (signature :named)
                   :when (not (has-value? used-named (parameter :label)))]
            {:label (parameter :label) :kind 14
             :detail (string "Named argument for " (signature :name))
             :insertText (string (parameter :label) " ")
             :sortText (rank 0 (get usage (parameter :label) 0)
                             (parameter :label))})
          @[])
        binding (binding-context content cursor call stack)
        module? (module-context? call prefix)
        metadata? (metadata-context? content token call prefix)
        table-key? (or (table-key-context? content cursor stack)
                       (table-key-call-context? call))
        locals (parser/get-syms-at-loc location content
                                       (get-in document [:analysis :syntax-tree]))
        globals (seq [name :in (all-bindings (document :eval-env))]
                  (binding-item name (document :eval-env)))
        imports (visible-imports document cursor)
        imported (imported-items workspace document imports usage)
        visible (visible-set (array ;locals ;imported ;globals))
        only (only-items state workspace document token call stack usage)
        bare-call?
        (when-let [entry (last stack)]
          (and (= 40 (entry :delimiter))
               (= 0 (get-in (direct-state content (entry :index) cursor) [:forms]))))
        raw-items
        (cond
          (= :comment mode) @[]
          module? (module-items state workspace document token call usage)
          (not= :code mode) @[]
          binding (binding-items binding prefix)
          only only
          metadata? (keyword-items workspace prefix usage true)
          (or table-key? (string/has-prefix? ":" prefix))
          (utils/concat-dedup-by-label
            named-items (keyword-items workspace prefix usage false))
          (let [ranked-locals
                (map |(merge $ {:sortText (rank 0 (get usage (string ($ :label)) 0)
                                                   (string ($ :label)))}) locals)
                ranked-globals
                (map |(merge $ {:sortText (rank 2 (get usage (string ($ :label)) 0)
                                                    (string ($ :label)))}) globals)]
            (utils/concat-dedup-by-label
              named-items
              ranked-locals
              imported
              (if (state :completion-snippets)
                (snippet-items content token prefix call bare-call?) @[])
              ranked-globals
              (if (>= (token :start)
                      (lookup/to-index
                        (import-position (get-in document [:analysis :index]) content)
                        content))
                (auto-import-items state workspace document token prefix visible usage)
                @[]))))
        matching
        (if (or module? (empty? prefix))
          raw-items
          (filter |(string/has-prefix? prefix (string ($ :label))) raw-items))
        limit 2000
        items (array/slice (sort-by |(or ($ :sortText) (string ($ :label))) matching)
                           0 (min limit (length matching)))]
    {:isIncomplete (> (length matching) limit)
     :items
     (map |(merge $ {:data {:uri (document :uri)
                            :version (document :version)
                            :snapshot (get-in document [:analysis :key])
                            :binding (string ($ :label))}})
          items)}))
