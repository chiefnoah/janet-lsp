(import ./index)
(import ./lookup)
(import ./position)
(import ./server-utils)
(import ./uri)
(import spork/path)

(def open-to-close {40 41 91 93 123 125})
(def close-to-open {41 40 93 91 125 123})
(def link-heads {"import" true "import*" true "use" true
                 "require" true "dofile" true})
(def reader-macros [39 44 59 124 126])

(defn- whitespace? [byte]
  (has-value? [0 9 10 11 12 13 32] byte))

(defn- token-separator? [byte]
  (or (whitespace? byte)
      (has-value? [34 35 39 40 41 44 59 91 93 96 123 124 125 126] byte)))

(defn- parent-id [stack]
  (and (not (empty? stack)) ((last stack) :id)))

(defn- parsed-string [source start end fallback]
  (try
    (let [value (parse (string/slice source start end))]
      (if (string? value) value fallback))
    ([_] fallback)))

(defn- reader-wrappers [readers outer-parent form-id]
  (def wrappers @[])
  (var parent outer-parent)
  (eachp [reader-index reader] readers
    (def id (string "reader:" form-id ":" reader-index))
    (array/push wrappers {:id id :start (reader :start)
                          :parent parent :reader-kind (reader :kind)})
    (set parent id))
  [wrappers parent])

(defn scan [source]
  "Scan source structure without evaluating it. Ranges use absolute byte indexes."
  (def bytes (string/bytes source))
  (def forms @[])
  (def tokens @[])
  (def comments @[])
  (def stack @[])
  (var state :code)
  (var escaped false)
  (var start 0)
  (var content-start 0)
  (var parent nil)
  (var token-readers @[])
  (var delimiter 0)
  (var pending-readers @[])
  (var index 0)
  (while (< index (length bytes))
    (def byte (bytes index))
    (case state
      :comment
      (when (= byte 10)
        (array/push comments {:start start :end index})
        (set state :code))

      :string
      (cond
        escaped (set escaped false)
        (= byte 92) (set escaped true)
        (= byte 34)
        (do
          (array/push
            tokens
            {:start content-start :end index :literal-start start
             :literal-end (inc index) :parent parent :kind :string
             :readers token-readers
             :complete true
             :value (parsed-string source start (inc index)
                                   (string/slice source content-start index))})
          (set state :code)))

      :long-string
      (when (= byte 96)
        (var count 0)
        (while (and (< (+ index count) (length bytes))
                    (= 96 (bytes (+ index count))))
          (+= count 1))
        (if (>= count delimiter)
          (do
            (def literal-end (+ index delimiter))
            (array/push
              tokens
              {:start content-start :end index :literal-start start
               :literal-end literal-end :parent parent :kind :string
               :readers token-readers
               :complete true
               :value (parsed-string source start literal-end
                                     (string/slice source content-start index))})
            (set state :code)
            (+= index (dec delimiter)))
          (+= index (dec count))))

      (cond
        (= byte 35)
        (do (set start index) (set state :comment))

        (= byte 34)
        (do
          (set start index)
          (set content-start (inc index))
          (set parent (parent-id stack))
          (set token-readers pending-readers)
          (set escaped false)
          (set pending-readers @[])
          (set state :string))

        (= byte 96)
        (do
          (var count 0)
          (while (and (< (+ index count) (length bytes))
                      (= 96 (bytes (+ index count))))
            (+= count 1))
          (set start index)
          (set content-start (+ index count))
          (set parent (parent-id stack))
          (set token-readers pending-readers)
          (set delimiter count)
          (set pending-readers @[])
          (set state :long-string)
          (+= index (dec count)))

        (has-value? reader-macros byte)
        (array/push pending-readers {:start index :kind byte})

        (and (= byte 64) (< (inc index) (length bytes))
             (get open-to-close (bytes (inc index))))
        nil

        (get open-to-close byte)
        (let [array-prefix? (and (> index 0) (= 64 (bytes (dec index))))
              form-start (or (and array-prefix? (dec index)) index)
              outer-parent (parent-id stack)
              [wrappers form-parent]
              (reader-wrappers pending-readers outer-parent index)]
          (array/push stack {:id index :start form-start :open byte
                             :parent form-parent
                             :reader-wrappers wrappers
                             :reader false :reader-kind nil
                             :callable (not array-prefix?)})
          (set pending-readers @[]))

        (get close-to-open byte)
        (when (and (not (empty? stack))
                   (= (get close-to-open byte) ((last stack) :open)))
          (def opened (array/pop stack))
          (array/push forms {:id (opened :id) :start (opened :start)
                             :end (inc index) :open (opened :open)
                             :parent (opened :parent)
                             :reader (opened :reader)
                             :reader-kind (opened :reader-kind)
                             :callable (opened :callable)
                             :complete true})
          (each wrapper (opened :reader-wrappers)
            (array/push forms {:id (wrapper :id) :start (wrapper :start)
                               :end (inc index) :open 0
                               :parent (wrapper :parent)
                               :reader true
                               :reader-kind (wrapper :reader-kind)
                               :callable true :complete true})))

        (not (token-separator? byte))
        (do
          (def token-start index)
          (def readers pending-readers)
          (while (and (< index (length bytes))
                      (not (token-separator? (bytes index))))
            (+= index 1))
          (array/push tokens
                      {:start token-start :end index
                       :literal-start token-start :literal-end index
                       :readers readers
                       :parent (parent-id stack) :kind :symbol :complete true
                       :value (string/slice source token-start index)})
          (set pending-readers @[])
          (-= index 1))))
    (+= index 1))

  (case state
    :comment (array/push comments {:start start :end (length source)})
    :string (array/push tokens
                        {:start content-start :end (length source)
                         :literal-start start :literal-end (length source)
                         :readers token-readers
                         :parent parent :kind :string :complete false
                         :value (string/slice source content-start)})
    :long-string (array/push tokens
                             {:start content-start :end (length source)
                              :literal-start start :literal-end (length source)
                              :readers token-readers
                              :parent parent :kind :string :complete false
                              :value (string/slice source content-start)}))
  (each opened stack
    (array/push forms {:id (opened :id) :start (opened :start)
                       :end (length source) :open (opened :open)
                       :parent (opened :parent)
                       :reader (opened :reader)
                       :reader-kind (opened :reader-kind)
                       :callable (opened :callable)
                       :complete false})
    (each wrapper (opened :reader-wrappers)
      (array/push forms {:id (wrapper :id) :start (wrapper :start)
                         :end (length source) :open 0
                         :parent (wrapper :parent)
                         :reader true
                         :reader-kind (wrapper :reader-kind)
                         :callable true :complete false})))
  {:forms forms :tokens tokens :comments comments})

(defn- index-range [source start end]
  {:start (lookup/from-index start source)
   :end (lookup/from-index end source)})

(defn- lsp-index-range [state source start end]
  (server-utils/lsp-range state source (index-range source start end)))

(defn- contains-index? [span cursor]
  (and (<= (span :start) cursor) (< cursor (span :end))))

(defn- range-key [span]
  (string (span :start) ":" (span :end)))

(defn- token-selection-spans [tokens]
  (def spans @[])
  (each token tokens
    (def start (if (= :string (token :kind))
                 (token :literal-start)
                 (token :start)))
    (def end (if (= :string (token :kind))
               (token :literal-end)
               (token :end)))
    (array/push spans {:start start :end end})
    (each reader (token :readers)
      (array/push spans {:start (reader :start) :end end})))
  spans)

(defn- selection-for [state source scanned byte-position]
  (def cursor (lookup/to-index byte-position source))
  (def spans
    (array
      ;(filter |(contains-index? $ cursor)
               (token-selection-spans (scanned :tokens)))
      ;(filter |(contains-index? $ cursor) (scanned :comments))
      ;(filter |(contains-index? $ cursor) (scanned :forms))
      ;[{:start 0 :end (length source)}]))
  (def seen @{})
  (def ordered
    (sort-by |[(- ($ :end) ($ :start)) (- ($ :start))]
             (filter |(if (get seen (range-key $))
                        false
                        (do (put seen (range-key $) true) true))
                     spans)))
  (var parent nil)
  (each span (reverse ordered)
    (def selection @{:range (lsp-index-range state source
                                              (span :start) (span :end))})
    (when parent (put selection :parent parent))
    (set parent selection))
  parent)

(defn on-selection-ranges [state params]
  (def document (server-utils/document state params))
  (def source (document :content))
  (def scanned (scan source))
  (def selections @[])
  (var valid true)
  (each requested (get params "positions" @[])
    (if-let [byte-position
             (position/lsp->byte-position source requested
                                          (state :position-encoding))]
      (array/push selections
                  (selection-for state source scanned byte-position))
      (set valid false)))
  [:ok state (if valid selections :null)])

(defn- comment-folds [source comments]
  (def full-line
    (filter
      (fn [span]
        (def location (lookup/from-index (span :start) source))
        (def line ((string/split "\n" source) (location :line)))
        (empty? (string/trim (string/slice line 0 (location :character)))))
      comments))
  (def folds @[])
  (var group-start nil)
  (var group-end nil)
  (var previous-line nil)
  (each span full-line
    (def line (get-in (lookup/from-index (span :start) source) [:line]))
    (unless (or (nil? previous-line) (= line (inc previous-line)))
      (when (and group-start
                 (> previous-line
                    (get-in (lookup/from-index group-start source) [:line])))
        (array/push folds {:start group-start :end group-end :kind "comment"}))
      (set group-start nil))
    (unless group-start (set group-start (span :start)))
    (set group-end (span :end))
    (set previous-line line))
  (when (and group-start
             (> previous-line
                (get-in (lookup/from-index group-start source) [:line])))
    (array/push folds {:start group-start :end group-end :kind "comment"}))
  folds)

(defn- folding-range [state source span line-only?]
  (def range (lsp-index-range state source (span :start) (span :end)))
  (def result @{:startLine (get-in range [:start :line])
                :endLine (get-in range [:end :line])})
  (unless line-only?
    (put result :startCharacter (get-in range [:start :character]))
    (put result :endCharacter (get-in range [:end :character])))
  (when-let [kind (span :kind)] (put result :kind kind))
  result)

(defn on-folding-ranges [state params]
  (def document (server-utils/document state params))
  (def source (document :content))
  (def scanned (scan source))
  (def spans
    (array
      ;(filter |(< (get-in (lookup/from-index ($ :start) source) [:line])
                   (get-in (lookup/from-index ($ :end) source) [:line]))
               (scanned :forms))
      ;(comment-folds source (scanned :comments))))
  (def ranges
    (sort-by |[($ :startLine) (or ($ :startCharacter) 0)
               (- ($ :endLine))]
             (map |(folding-range state source $ (state :folding-line-only))
                  spans)))
  (def limit (state :folding-range-limit))
  [:ok state (if (and (number? limit) (= limit (math/floor limit)) (>= limit 0)
                      (> (length ranges) limit))
               (array/slice ranges 0 limit)
               ranges)])

(defn- source-target [base-path module exact?]
  (when base-path
    (def base
      (if (path/abspath? module)
        module
        (path/abspath (path/join base-path module))))
    (def candidates
      (cond
        exact?
        [base]
        (= ".janet" (path/ext base))
        [base]
        (nil? (path/ext base))
        [(string base ".janet") (path/join base "init.janet")]
        @[]))
    (when-let [target (first (filter |(and (os/stat $)
                                           (not= :directory (os/stat $ :mode)))
                                      candidates))]
      (uri/path->file-uri target))))

(defn- indexed-module-target [workspace document module]
  (if (string/has-prefix? "." module)
    (when-let [document-path (document :path)]
      (def base (path/abspath (path/join (path/dirname document-path) module)))
      (def candidates
        (cond
          (= ".janet" (path/ext base)) [base]
          (nil? (path/ext base))
          [(string base ".janet") (path/join base "init.janet")]
          @[]))
      (first (filter |(get (workspace :index) $)
                     (map uri/path->file-uri candidates))))
    (index/module-uri workspace (document :uri) module)))

(defn- link-target [workspace document head module]
  (if (= "dofile" head)
    (source-target (or (workspace :path) (os/cwd)) module true)
    (or (indexed-module-target workspace document module)
        (when-let [document-path (document :path)]
          (source-target (path/dirname document-path) module false)))))

(defn- direct-tokens [scanned form]
  (sort-by |($ :start)
           (filter |(= (form :id) ($ :parent)) (scanned :tokens))))

(defn- executable-form? [forms-by-id heads-by-id form]
  (var current form)
  (def ancestry @[])
  (while current
    (array/push ancestry current)
    (set current (get forms-by-id (current :parent))))
  (var executable true)
  (var quasiquote-depth 0)
  (each ancestor (reverse ancestry)
    (def head (get heads-by-id (ancestor :id)))
    (def reader-kind (ancestor :reader-kind))
    (cond
      (or (= 39 reader-kind) (= "quote" head))
      (set executable false)

      (or (= 126 reader-kind) (= "quasiquote" head))
      (+= quasiquote-depth 1)

      (or (has-value? [44 59] reader-kind)
          (has-value? ["unquote" "splice"] head))
      (when (> quasiquote-depth 0) (-= quasiquote-depth 1))))
  (and executable (= 0 quasiquote-depth)))

(defn on-document-links [state params]
  (def document (server-utils/document state params))
  (def source (document :content))
  (def scanned (scan source))
  (def workspace (server-utils/document-workspace state document))
  (def links @[])
  (def forms-by-id @{})
  (def heads-by-id @{})
  (each form (scanned :forms) (put forms-by-id (form :id) form))
  (each form (scanned :forms)
    (when-let [head (get-in (direct-tokens scanned form) [0 :value])]
      (put heads-by-id (form :id) head)))
  (each form (filter |(and (= 40 ($ :open))
                           ($ :callable)
                           (executable-form? forms-by-id heads-by-id $))
                     (scanned :forms))
    (def tokens (direct-tokens scanned form))
    (when (and (not (empty? tokens)) (get link-heads (get-in tokens [0 :value])))
      (def head (get-in tokens [0 :value]))
      (def arguments
        (if (= "use" head)
          (array/slice tokens 1)
          (array/slice tokens 1 (min 2 (length tokens)))))
      (each argument arguments
        (when-let [target (and (argument :complete)
                               (empty? (argument :readers))
                               (or (has-value? ["import" "import*" "use"] head)
                                   (= :string (argument :kind)))
                               (link-target workspace document head
                                            (argument :value)))]
          (def link
            @{:range (lsp-index-range state source
                                      (argument :start) (argument :end))
              :target target})
          (when (state :document-link-tooltips)
            (put link :tooltip "Open Janet source"))
          (array/push links link)))))
  [:ok state (sort-by |[(get-in $ [:range :start :line])
                        (get-in $ [:range :start :character])]
                      links)])
