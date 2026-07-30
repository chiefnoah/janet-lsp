(import ./document-features)
(import ./index)
(import ./lookup)
(import ./position)
(import ./request-control)
(import ./server-utils)

(def special-forms
  {"break" true "def" true "def-" true "defglobal" true
   "defmacro" true "defmacro-" true "defn" true "defn-" true
   "do" true "fn" true "if" true "let" true "loop" true
   "import" true "import*" true "use" true "require" true "dofile" true
   "nil" true "true" true "false" true
   "quasiquote" true "quote" true "set" true "splice" true
   "unquote" true "upscope" true "var" true "var-" true
   "varglobal" true "varfn" true "varfn-" true "while" true})

(def operators
  {"+" true "-" true "*" true "/" true "%" true "=" true
   "<" true ">" true "<=" true ">=" true "not=" true})

(defn- range-key [range]
  [(get-in range [:start :line]) (get-in range [:start :character])
   (get-in range [:end :line]) (get-in range [:end :character])])

(defn- range-id [range]
  (string (get-in range [:start :line]) ":"
          (get-in range [:start :character]) ":"
          (get-in range [:end :line]) ":"
          (get-in range [:end :character])))

(defn- in-range? [position range]
  (let [line (position :line) character (position :character)
        start (range :start) end (range :end)]
    (and (or (> line (start :line))
             (and (= line (start :line)) (>= character (start :character))))
         (or (< line (end :line))
             (and (= line (end :line)) (<= character (end :character)))))))

(defn- definition-type [definition]
  (cond
    (string/has-prefix? "defmacro" (definition :form)) 3
    (= 12 (definition :kind)) 2
    4))

(defn records [record env content &opt workspace state request-id]
  (def definitions (record :definitions))
  (def found @[])
  (def occupied @{})
  (def found-ranges @{})
  (def parameter-identities @{})
  (def local-first @{})
  (def references-by-range @{})
  (def definitions-by-identity @{})
  (def line-starts (lookup/line-starts content))

  (var work 0)
  (each reference (record :references)
    (when (and state (= 0 (% work 256)))
      (request-control/checkpoint state request-id))
    (+= work 1)
    (put references-by-range (range-id (reference :range)) reference))
  (each definition (if workspace (index/definitions workspace) definitions)
    (put definitions-by-identity (definition :identity) definition))
  (each definition definitions
    (when (and state (= 0 (% work 256)))
      (request-control/checkpoint state request-id))
    (+= work 1)
    (put definitions-by-identity (definition :identity) definition))

  (each definition definitions
    (def modifiers
      (bor 3 (if (has-value? ["def" "def-" "defglobal"] (definition :form)) 4 0)))
    (array/push found {:range (definition :selection-range)
                       :type (definition-type definition)
                       :modifiers modifiers})
    (put occupied (range-id (definition :selection-range)) true)
    (put found-ranges (range-id (definition :selection-range)) true)
    (each parameter (definition :children)
      (when-let [reference
                 (get references-by-range (range-id (parameter :selection-range)))]
        (when (reference :identity)
          (put parameter-identities (reference :identity) true)
          (put local-first (reference :identity) true)))
      (array/push found {:range (parameter :selection-range)
                         :type 5 :modifiers 1})
      (put occupied (range-id (parameter :selection-range)) true)
      (put found-ranges (range-id (parameter :selection-range)) true)))

  (each reference (record :references)
    (when (and state (= 0 (% work 256)))
      (request-control/checkpoint state request-id))
    (+= work 1)
    (def range (reference :range))
    (unless (get occupied (range-id range))
      (def name (reference :name))
      (def identity (reference :identity))
      (def target (and identity (get definitions-by-identity identity)))
      (def local? (= :local (reference :identity-kind)))
      (def parameter? (and local? (get parameter-identities identity)))
      (def declaration? (and local? (not (get local-first identity))))
      (when local? (put local-first identity true))
      (def binding (get env (symbol name) nil))
      (def module-syntax?
        (any? (map |(and (in-range? (get-in range [:start]) ($ :range))
                         (or (= name ($ :module)) (= name ($ :alias))))
                   (record :imports))))
      (array/push
        found
        {:range range
         :type
         (cond
           parameter? 5
           local? 4
           module-syntax? 0
           (string/has-prefix? ":" name) 6
           (scan-number name) 8
           (get special-forms name) 6
           (get operators name) 10
           target (definition-type target)
           (and (string/find "/" name) (not identity)) 0
           (and binding (binding :macro)) 3
           (and binding (has-value? [:function :cfunction]
                                    (type (binding :value)))) 2
           4)
         :modifiers
         (cond
            declaration? 1
            (and binding (nil? target) (not local?)) 8
            0)})
      (put found-ranges (range-id range) true)))

  (def scanned (document-features/scan content))
  (each token (scanned :tokens)
    (when (has-value? [:string :number] (token :kind))
      (def range {:start (lookup/from-index (token :literal-start) content line-starts)
                  :end (lookup/from-index (token :literal-end) content line-starts)})
      (unless (get found-ranges (range-id range))
        (array/push found {:range range
                           :type (if (= :string (token :kind)) 7 8)
                           :modifiers 0})
        (put found-ranges (range-id range) true))))
  (each span (scanned :comments)
    (array/push found
                {:range {:start (lookup/from-index (span :start) content line-starts)
                         :end (lookup/from-index (span :end) content line-starts)}
                 :type 9 :modifiers 0}))

  (def sorted (sort-by |(range-key ($ :range)) found))
  (def result @[])
  (each token sorted
    (def previous (last result))
    (unless (and previous
                 (= (get-in previous [:range :end :line])
                    (get-in token [:range :start :line]))
                 (> (get-in previous [:range :end :character])
                    (get-in token [:range :start :character])))
      (array/push result token)))
  result)

(defn encode [state content records &opt requested request-id]
  (def data @[])
  (var previous-line 0)
  (var previous-character 0)
  (var count 0)
  (def range-start (and requested (or (get requested "start")
                                      (get requested :start))))
  (def range-end (and requested (or (get requested "end")
                                    (get requested :end))))
  (defn position-before? [left right]
    (def left-line (or (get left "line") (get left :line)))
    (def right-line (or (get right "line") (get right :line)))
    (def left-character (or (get left "character") (get left :character) 0))
    (def right-character (or (get right "character") (get right :character) 0))
    (or (< left-line right-line)
        (and (= left-line right-line) (< left-character right-character))))
  (def lines (string/split "\n" content))
  (each token records
    (when (= 0 (% count 256))
      (request-control/checkpoint state request-id))
    (+= count 1)
    (def range (server-utils/lsp-range state content (token :range)))
    (def start (range :start))
    (def end (range :end))
    (when (and start end)
      (for line (start :line) (inc (end :line))
        (def segment-start
          {:line line :character (if (= line (start :line))
                                   (start :character) 0)})
        (def segment-end
          {:line line
           :character
           (if (= line (end :line))
             (end :character)
             (let [text (get lines line "")]
               (position/byte-to-units text (length text)
                                       (state :position-encoding))))})
        (when (and (> (segment-end :character) (segment-start :character))
                   (or (nil? requested)
                       (and (position-before? segment-start range-end)
                            (position-before? range-start segment-end))))
          (def delta-line (- line previous-line))
          (def delta-character (if (= delta-line 0)
                                 (- (segment-start :character) previous-character)
                                 (segment-start :character)))
          (array/concat data [delta-line delta-character
                              (- (segment-end :character)
                                 (segment-start :character))
                              (token :type) (token :modifiers)])
          (set previous-line line)
          (set previous-character (segment-start :character))))))
  data)

(defn result-id [snapshot encoding data]
  (string (snapshot :key) ":" encoding ":"
          (index/content-hash (string/format "%j" data))))

(defn delta [previous current]
  (var prefix 0)
  (def previous-length (length previous))
  (def current-length (length current))
  (while (and (< prefix previous-length) (< prefix current-length)
              (= (previous prefix) (current prefix)))
    (+= prefix 1))
  (var suffix 0)
  (while (and (< suffix (- previous-length prefix))
              (< suffix (- current-length prefix))
              (= (previous (- previous-length suffix 1))
                 (current (- current-length suffix 1))))
    (+= suffix 1))
  (if (and (= prefix previous-length) (= prefix current-length))
    @[]
    [{:start prefix
      :deleteCount (- previous-length prefix suffix)
      :data (array/slice current prefix (- current-length suffix))}]))
