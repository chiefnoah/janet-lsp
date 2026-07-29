(def categories
  {"parse" true
   "compile" true
   "runtime" true
   "analysis" true
    "unusedParameter" true
    "unusedBinding" true
    "unusedImport" true
   "calls" true
   "undefinedSymbol" true
   "duplicateDefinition" true
   "shadowing" true
   "unreachableCode" true
   "constantCondition" true})

(def severity-names
  {"error" 1 "warning" 2 "information" 3 "info" 3 "hint" 4 "off" false})

(defn category [code]
  (cond
    (string/has-prefix? "janet.parse" code) "parse"
    (string/has-prefix? "janet.compile" code) "compile"
    (string/has-prefix? "janet.runtime" code) "runtime"
    (string/has-prefix? "janet.call." code) "calls"
    (= code "janet.lint.unused-parameter") "unusedParameter"
    (= code "janet.lint.unused-binding") "unusedBinding"
    (= code "janet.lint.unused-import") "unusedImport"
    (= code "janet.lint.undefined-symbol") "undefinedSymbol"
    (= code "janet.lint.duplicate-definition") "duplicateDefinition"
    (= code "janet.lint.shadowing") "shadowing"
    (= code "janet.lint.unreachable-code") "unreachableCode"
    (= code "janet.lint.constant-condition") "constantCondition"
    "analysis"))

(defn- severity [value]
  (cond
    (and (number? value) (= value (math/floor value)) (<= 1 value 4)) value
    (string? value) (get severity-names (string/ascii-lower value))
    (= false value) false
    nil))

(defn diagnostics [options]
  (def supplied
    (or (get options "diagnostics")
        (get-in options ["janetLsp" "diagnostics"])
        (get-in options ["janet-lsp" "diagnostics"])
        {}))
  (def settings @{})
  (when (dictionary? supplied)
    (eachp [name value] supplied
      (def found (severity value))
      (when (and (get categories name) (not (nil? found)))
        (put settings name found))))
  settings)

(defn apply-severity [result settings]
  (def diagnostic-category (category (or (result :code) "janet.analysis")))
  (if (has-key? settings diagnostic-category)
    (when-let [configured (get settings diagnostic-category)]
      (merge result {:severity configured}))
    result))

(defn- copy-set [values]
  (def copied @{})
  (eachp [key value] values (when value (put copied key true)))
  copied)

(defn- directive [line]
  (def trimmed (string/trim line))
  (def prefix "# janet-lsp:")
  (when (string/has-prefix? prefix trimmed)
    (def body (string/trim (string/slice trimmed (length prefix))))
    (def normalized (string/replace-all "," " " body))
    (def parts
      (filter |(not (empty? $))
              (string/split " " normalized)))
    (when (not (empty? parts))
      [(first parts) (array/slice parts 1)])))

(defn- matches? [suppressed code]
  (or (get suppressed "all")
      (get suppressed code)
      (get suppressed (category code))))

(defn- comment-lines [source]
  (def bytes (string/bytes source))
  (def found @{})
  (var index 0)
  (var line 0)
  (var state :code)
  (var escaped false)
  (var delimiter 0)
  (while (< index (length bytes))
    (def byte (bytes index))
    (case state
      :string
      (cond
        (= byte 10) (+= line 1)
        escaped (set escaped false)
        (= byte 92) (set escaped true)
        (= byte 34) (set state :code))

      :long-string
      (if (= byte 96)
        (do
          (var count 0)
          (while (and (< (+ index count) (length bytes))
                      (= 96 (bytes (+ index count))))
            (+= count 1))
          (when (= count delimiter) (set state :code))
          (+= index (dec count)))
        (when (= byte 10) (+= line 1)))

      (cond
        (= byte 35)
        (do
          (def start index)
          (while (and (< index (length bytes)) (not= 10 (bytes index)))
            (+= index 1))
          (put found line (string/slice source start index))
          (when (< index (length bytes)) (+= line 1)))
        (= byte 34) (set state :string)
        (= byte 96)
        (do
          (var count 0)
          (while (and (< (+ index count) (length bytes))
                      (= 96 (bytes (+ index count))))
            (+= count 1))
          (set delimiter count)
          (set state :long-string)
          (+= index (dec count)))
        (= byte 10) (+= line 1)))
    (+= index 1))
  found)

(defn suppress [results source]
  (var disabled @{})
  (def disabled-at @[])
  (def ignored @{})
  (def comments (comment-lines source))
  (eachp [line-number line] (string/split "\n" source)
    (when-let [comment (get comments line-number)
               [action names] (directive comment)]
      (def targets (if (empty? names) ["all"] names))
      (case action
        "disable" (each name targets (put disabled name true))
        "enable" (if (has-value? targets "all")
                   (set disabled @{})
                   (each name targets (put disabled name nil)))
        "ignore-next-line" (put ignored (inc line-number)
                                 (reduce (fn [set name] (put set name true) set)
                                         @{} targets))))
    (array/push disabled-at (copy-set disabled)))
  (filter
    (fn [result]
      (let [line (max 0 (dec (get-in result [:location 0] 1)))
            code (or (result :code) "janet.analysis")]
        (not (or (matches? (get disabled-at line @{}) code)
                 (matches? (get ignored line @{}) code)))))
    results))
