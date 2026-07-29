(import ./completion)
(import ./document-features)
(import ./index)
(import ./lookup)
(import ./parser)
(import ./position)
(import ./server-utils)

(defn- fresh? [document diagnostic]
  (and (= (index/content-hash (document :content))
          (get-in diagnostic ["data" "contentHash"]))
       (= (document :version) (get-in diagnostic ["data" "version"]))))

(defn- requested? [only kind]
  (or (empty? only)
      (any? (map |(or (= $ kind) (string/has-prefix? (string $ ".") kind)) only))))

(defn- versioned-action [state document diagnostic title edits &opt preferred]
  {:title title :kind "quickfix" :diagnostics [diagnostic]
   :isPreferred (not (not preferred))
   :edit {:documentChanges
          [(server-utils/versioned-edit state (document :uri) edits)]}})

(defn- missing-delimiters [source]
  (def stack @[])
  (each byte (string/bytes (lookup/structure-mask source))
    (case byte
      40 (array/push stack 41)
      91 (array/push stack 93)
      123 (array/push stack 125)
      41 (if (and (not (empty? stack)) (= 41 (last stack)))
           (array/pop stack) (array/push stack nil))
      93 (if (and (not (empty? stack)) (= 93 (last stack)))
           (array/pop stack) (array/push stack nil))
      125 (if (and (not (empty? stack)) (= 125 (last stack)))
            (array/pop stack) (array/push stack nil))))
  (when (not (has-value? stack nil))
    (string/from-bytes ;(reverse stack))))

(defn- byte-range [state content lsp-range]
  (when-let [start (position/lsp->byte-position content
                                                 (or (get lsp-range "start")
                                                     (get lsp-range :start))
                                                 (state :position-encoding))
             end (position/lsp->byte-position content
                                               (or (get lsp-range "end")
                                                   (get lsp-range :end))
                                               (state :position-encoding))]
    {:start (lookup/to-index start content) :end (lookup/to-index end content)}))

(defn- lsp-edit [state content start end new-text]
  {:range (server-utils/lsp-range
            state content
            {:start (lookup/from-index start content)
             :end (lookup/from-index end content)})
   :newText new-text})

(defn- span-for-token [token]
  {:start (if (empty? (token :readers))
            (token :literal-start)
            (get-in token [:readers 0 :start]))
   :end (token :literal-end)
   :value (token :value) :kind (token :kind)})

(defn- call-at [state content diagnostic scanned]
  (when-let [range (byte-range state content (get diagnostic "range"))]
    (def start (range :start))
    (when-let [form
               (first (sort-by |(- ($ :end) ($ :start))
                               (filter |(and ($ :complete) (= start ($ :start))
                                             (= 40 ($ :open)))
                                       (scanned :forms))))]
      (def id (form :id))
      (def items
        (sort-by |[($ :start) ($ :end)]
                 (array
                   ;(map span-for-token
                         (filter |(= id ($ :parent)) (scanned :tokens)))
                   ;(map |{:start ($ :start) :end ($ :end) :form true}
                         (filter |(and (= id ($ :parent)) (not ($ :reader)))
                                 (scanned :forms))))))
      {:form form :items items})))

(defn- unknown-named-action [state document diagnostic scanned]
  (def content (document :content))
  (def label (get-in diagnostic ["data" "label"]))
  (def positional (get-in diagnostic ["data" "positional"]))
  (def named-index (get-in diagnostic ["data" "named-index"]))
  (when-let [call (and (string? label) (call-at state content diagnostic scanned))]
    (def items (call :items))
    (def key-index (and (number? positional) (number? named-index)
                        (+ 1 positional named-index)))
    (when (and key-index (< key-index (length items))
               (= label (get-in items [key-index :value])))
      (when (and (> key-index 0) (< (inc key-index) (length items)))
        (def start (get-in items [key-index :start]))
        (def end (get-in items [(inc key-index) :end]))
        (unless (any? (map |(and (< ($ :start) end) (< start ($ :end)))
                           (scanned :comments)))
          (versioned-action state document diagnostic
                            (string "Remove unsupported " label " argument")
                            [(lsp-edit state content start end "")] false))))))

(defn- missing-arguments-action [state document diagnostic scanned]
  (def content (document :content))
  (def missing (get-in diagnostic ["data" "missing"]))
  (def provided (get-in diagnostic ["data" "provided"]))
  (when-let [call (and (number? missing) (> missing 0)
                       (number? provided)
                       (call-at state content diagnostic scanned))]
    (def next-argument (get-in call [:items (+ 1 provided)]))
    (def insertion
      (if next-argument (next-argument :start) (dec (get-in call [:form :end]))))
    (def placeholders (string/join (map (fn [_] "nil") (range missing)) " "))
    (def text
      (if next-argument (string placeholders " ") (string " " placeholders)))
    (versioned-action state document diagnostic
                      (string "Insert " missing " missing argument"
                              (if (= missing 1) "" "s"))
                      [(lsp-edit state content insertion insertion text)] true)))

(defn- underscore-action [state document diagnostic scanned parameter?]
  (def content (document :content))
  (def name (get-in diagnostic ["data" "name"]))
  (def target
    (if parameter?
      (when-let [call (call-at state content diagnostic scanned)]
        (when-let [parameters
                   (first (filter |(and (= 91 ($ :open))
                                       (= (get-in call [:form :id]) ($ :parent)))
                                  (scanned :forms)))]
          (first (filter |(and (= name ($ :value))
                               (= (parameters :id) ($ :parent)))
                         (scanned :tokens)))))
      (when-let [range (byte-range state content (get diagnostic "range"))]
        {:start (range :start) :end (range :end)})))
  (when (and target
             (= name (string/slice content (target :start)
                                   (or (target :end)
                                       (+ (target :start) (length name)))))
             (not (any? (map |(= (string "_" name) ($ :value))
                             (scanned :tokens)))))
    (versioned-action state document diagnostic
                      (string "Mark " name " as intentionally unused")
                      [(lsp-edit state content (target :start) (target :start) "_")]
                      true)))

(defn- mark-import-unused-action [state document diagnostic scanned]
  (def content (document :content))
  (def alias (get-in diagnostic ["data" "alias"]))
  (def prefix (get-in diagnostic ["data" "prefix"]))
  (when-let [range (byte-range state content (get diagnostic "range"))]
    (def form
      (first (filter |(and (= (range :start) ($ :start))
                           (= (range :end) ($ :end)) (= 40 ($ :open)))
                     (scanned :forms))))
    (def tokens
      (sort-by |($ :literal-start)
               (filter |(and form (= (form :id) ($ :parent)))
                       (scanned :tokens))))
    (def option-index
      (find-index |(has-value? [":as" ":prefix"] ($ :value)) tokens))
    (var target (and option-index (get tokens (inc option-index))))
    (when (and target
               (not (if (= ":as" (get-in tokens [option-index :value]))
                      (= alias (target :value))
                      (= prefix (target :value)))))
      (set target nil))
    (def desired-prefix (string "_" prefix))
    (def workspace (server-utils/document-workspace state document))
    (def module (get-in diagnostic ["data" "module"]))
    (def target-uri (and module (index/module-uri workspace (document :uri) module)))
    (def exported (and target-uri (index/exported-definitions workspace target-uri)))
    (def desired-bindings
      (and exported (map |(string desired-prefix ($ :name)) exported)))
    (def existing-bindings
      (array
        ;(map |($ :name) (get-in document [:analysis :index :definitions] @[]))
        ;(catseq [imported :in (get-in document [:analysis :index :imports] @[])
                  :when (not (deep= (imported :range)
                                    {:start (lookup/from-index (range :start) content)
                                     :end (lookup/from-index (range :end) content)}))
                  :let [uri (index/module-uri workspace (document :uri)
                                              (imported :module))]
                  :when uri
                  definition :in (index/exported-definitions workspace uri)]
            (string (imported :prefix) (definition :name)))))
    (def collision
      (or (nil? desired-bindings)
          (any? (map |(has-value? existing-bindings $) desired-bindings))))
    (unless collision
      (def edit
        (if target
          (lsp-edit state content (target :start) (target :start) "_")
          (lsp-edit state content (dec (range :end)) (dec (range :end))
                    (string " :as _" alias))))
      (versioned-action state document diagnostic
                        "Mark import as intentionally unused" [edit] true))))

(defn- missing-import-action [state document diagnostic]
  (when-let [name (get-in diagnostic ["data" "name"])
             edit (completion/missing-import-edit state document name)]
    (versioned-action state document diagnostic
                      (string "Import " name) [edit] true)))

(defn- parse-forms [source]
  (try
    (let [state (parser/new) forms @[]]
      (parser/consume state source)
      (parser/eof state)
      (while (parser/has-more state)
        (array/push forms ((parser/produce state true) 0)))
      forms)
    ([_] nil)))

(varfn initialization-literal? [])

(varfn initialization-literal? [value]
  (cond
    (or (nil? value) (boolean? value) (number? value)
        (string? value) (keyword? value)) true
    (and (tuple? value) (not (empty? value)) (= 'quote (value 0))) true
    (and (tuple? value) (= :brackets (tuple/type value)))
    (all initialization-literal? value)
    (dictionary? value)
    (and (all initialization-literal? (keys value))
         (all initialization-literal? (values value)))
    false))

(defn- initialization-pure? [source]
  (when-let [forms (parse-forms source)]
    (all
      (fn [form]
        (and (tuple? form) (not (empty? form))
             (cond
               (has-value? ['defn 'defn- 'varfn 'varfn-
                            'defmacro 'defmacro-] (form 0)) true
               (has-value? ['def 'def- 'var 'var-] (form 0))
               (and (>= (length form) 3)
                    (all initialization-literal? (tuple/slice form 2)))
               false)))
      forms)))

(defn- import-spans [state document]
  (def content (document :content))
  (def lines (string/split "\n" content))
  (def workspace (server-utils/document-workspace state document))
  (def seen @{})
  (sort-by |($ :start)
           (catseq [imported :in (get-in document [:analysis :index :imports] @[])
                    :when (and (imported :top-level)
                               (= "import" (imported :kind))
                               (not (imported :export))
                               (not (empty? (imported :prefix)))
                               (= 0 (get-in imported [:range :start :character]))
                               (or (= 0 (get-in imported [:range :start :line]))
                                   (not
                                     (string/has-prefix?
                                       "#"
                                       (string/trim
                                         (get lines
                                              (dec (get-in imported
                                                           [:range :start :line])))))))
                               (empty?
                                 (string/trim
                                   (string/slice
                                     (get lines (get-in imported [:range :end :line]))
                                     (get-in imported [:range :end :character])))))
                    :let [target-uri
                          (index/module-uri workspace (document :uri)
                                            (imported :module))
                          target-content
                          (and target-uri (server-utils/content state target-uri))]
                    :when (and target-content (initialization-pure? target-content))
                    :let [start (lookup/to-index (get-in imported [:range :start]) content)
                          end (lookup/to-index (get-in imported [:range :end]) content)
                          key (string start ":" end)]
                    :when (if (get seen key) false (do (put seen key true) true))]
             {:start start :end end
              :prefix (imported :prefix)
              :module (imported :module)
              :source (string/slice content start end)})))

(defn- import-blocks [content spans]
  (def blocks @[])
  (each span spans
    (if (empty? blocks)
      (array/push blocks @[span])
      (let [block (last blocks) previous (last block)
            between (string/slice content (previous :end) (span :start))]
        (if (empty? (string/trim between))
          (array/push block span)
          (array/push blocks @[span])))))
  blocks)

(defn- source-action [state document kind title]
  (def content (document :content))
  (def newline (if (string/find "\r\n" content) "\r\n" "\n"))
  (def edits @[])
  (each block (import-blocks content (import-spans state document))
    (def prefixes (map |($ :prefix) block))
    (def overlapping-prefixes
      (any? (map (fn [left]
                   (any? (map |(and (not= left $)
                                    (or (string/has-prefix? left $)
                                        (string/has-prefix? $ left)))
                              prefixes)))
                 prefixes)))
    (when (and (= (length block) (length (distinct (map |($ :module) block))))
               (not overlapping-prefixes)
               (not (any? (map |(has-value? (try (parse ($ :source)) ([_] @[]))
                                             :fresh)
                                block))))
      (def sorted (sort-by |(string/ascii-lower ($ :source)) block))
      (def replacement (string/join (map |($ :source) sorted) newline))
      (def start (get-in block [0 :start]))
      (def end ((last block) :end))
      (unless (= replacement (string/slice content start end))
        (array/push edits (lsp-edit state content start end replacement)))))
  (when (not (empty? edits))
    {:title title :kind kind
     :edit {:documentChanges
            [(server-utils/versioned-edit state (document :uri) edits)]}}))

(defn on-code-action [state params]
  (def document (server-utils/document state params))
  (def only (get-in params ["context" "only"] @[]))
  (def actions @[])
  (def scanned (document-features/scan (document :content)))
  (when (requested? only "quickfix")
    (each diagnostic (get-in params ["context" "diagnostics"] @[])
      (when (fresh? document diagnostic)
        (def action
          (case (get diagnostic "code")
            "janet.parse.unclosed-delimiter"
            (when-let [closing (missing-delimiters (document :content))]
              (when (not (empty? closing))
                (def end (position/document-end (document :content)
                                                (state :position-encoding)))
                (versioned-action state document diagnostic
                                  (string "Insert missing " closing)
                                  [{:range {:start end :end end} :newText closing}] true)))
            "janet.call.unknown-named-argument"
            (unknown-named-action state document diagnostic scanned)
            "janet.call.missing-arguments"
            (missing-arguments-action state document diagnostic scanned)
            "janet.lint.unused-parameter"
            (underscore-action state document diagnostic scanned true)
            "janet.lint.unused-binding"
            (underscore-action state document diagnostic scanned false)
            "janet.lint.unused-import"
            (mark-import-unused-action state document diagnostic scanned)
            "janet.lint.undefined-symbol"
            (missing-import-action state document diagnostic)
            nil))
        (when action (array/push actions action)))))
  (when (requested? only "source.sortImports")
    (when-let [action (source-action state document "source.sortImports"
                                    "Sort imports")]
      (array/push actions action)))
  (when (requested? only "source.organizeImports")
    (when-let [action (source-action state document "source.organizeImports"
                                    "Organize imports")]
      (array/push actions action)))
  [:ok state actions])
