(import ./logging)

(varfn to-index [])

(defn lookup [{:line line :character character} source]
  (string/from-bytes (((string/split "\n" source) line) character)))

(defn code-mask [source]
  "Replace strings and comments with spaces while preserving byte offsets and newlines."
  (def bytes (string/bytes source))
  (def masked @"")
  (var state :code)
  (var escaped false)
  (each byte bytes
    (case state
      :comment
      (if (= byte 10)
        (do (buffer/push-byte masked byte) (set state :code))
        (buffer/push-byte masked 32))

      :string
      (cond
        (= byte 10) (buffer/push-byte masked byte)
        escaped (do (buffer/push-byte masked 32) (set escaped false))
        (= byte 92) (do (buffer/push-byte masked 32) (set escaped true))
        (= byte 34) (do (buffer/push-byte masked 32) (set state :code))
        (buffer/push-byte masked 32))

      (cond
        (= byte 35) (do (buffer/push-byte masked 32) (set state :comment))
        (= byte 34) (do (buffer/push-byte masked 32) (set state :string))
        (buffer/push-byte masked byte))))
  (string masked))

(defn structure-mask [source]
  "Mask comments and string contents, retaining one placeholder per string form."
  (def bytes (string/bytes source))
  (def masked @"")
  (var state :code)
  (var escaped false)
  (each byte bytes
    (case state
      :comment
      (if (= byte 10)
        (do (buffer/push-byte masked byte) (set state :code))
        (buffer/push-byte masked 32))
      :string
      (cond
        (= byte 10) (buffer/push-byte masked byte)
        escaped (do (buffer/push-byte masked 32) (set escaped false))
        (= byte 92) (do (buffer/push-byte masked 32) (set escaped true))
        (= byte 34) (do (buffer/push-byte masked 32) (set state :code))
        (buffer/push-byte masked 32))
      (cond
        (= byte 35) (do (buffer/push-byte masked 32) (set state :comment))
        (= byte 34) (do (buffer/push-byte masked 120) (set state :string))
        (buffer/push-byte masked byte))))
  (string masked))

(defn call-context [location source]
  (def cursor (to-index location source))
  (def masked (structure-mask source))
  (def bytes (string/bytes masked))
  (def stack @[])
  (for i 0 (min cursor (length bytes))
    (case (bytes i)
      40 (array/push stack [40 i])
      91 (array/push stack [91 i])
      123 (array/push stack [123 i])
      41 (when (and (not (empty? stack)) (= 40 ((last stack) 0))) (array/pop stack))
      93 (when (and (not (empty? stack)) (= 91 ((last stack) 0))) (array/pop stack))
      125 (when (and (not (empty? stack)) (= 123 ((last stack) 0))) (array/pop stack))))
  (def call (last (filter |(= 40 ($ 0)) stack)))
  (when call
    (def start (inc (call 1)))
    (var depth 0)
    (var in-token false)
    (var forms 0)
    (var callee-start nil)
    (var callee-end nil)
    (for i start (min cursor (length bytes))
      (def byte (bytes i))
      (def whitespace? (has-value? [9 10 11 12 13 32] byte))
      (when (and (= depth 0) (not whitespace?) (not in-token))
        (set in-token true)
        (+= forms 1)
        (when (= forms 1) (set callee-start i)))
      (when (and (= depth 0) whitespace? in-token)
        (set in-token false)
        (when (and (= forms 1) (nil? callee-end)) (set callee-end i)))
      (cond
        (has-value? [40 91 123] byte) (+= depth 1)
        (has-value? [41 93 125] byte) (when (> depth 0) (-= depth 1))))
    (when (and callee-start (nil? callee-end)) (set callee-end cursor))
    (when (and callee-start callee-end)
      {:callee (string/trim (string/slice masked callee-start callee-end))
       :active-parameter (max 0 (- forms 2))
       :range [start cursor]})))

(defn enclosing-call-heads [location source]
  (def cursor (to-index location source))
  (def masked (structure-mask source))
  (def bytes (string/bytes masked))
  (def stack @[])
  (for i 0 (min cursor (length bytes))
    (case (bytes i)
      40 (array/push stack i)
      41 (when (not (empty? stack)) (array/pop stack))))
  (map (fn [start]
         (first (peg/match '(* "(" :s* (<- (some (if-not (+ :s (set "()[]{}")) 1))))
                           (string/slice masked start))))
       stack))

(def word-peg
  (peg/compile
    ~{:s (set " \t\0\f\v") :s* (any :s) :s+ (some :s)
      :paren (/ (* (position) (set "()[]{}'\"`") (constant "") (position))
                ,|[$0 $1 (dec $2)])
      :ws (/ (* (position) :s+ (constant "") (position))
             ,|[$0 $1 (dec $2)])
      :word (/ (* (position)
                  (<- (some (if-not (set " ()[]{}`'\"") 1)))
                  (position)
                  (? (+ :s (set ")}]\""))))
               ,|[$0 $1 $2])
      :main (some (+ :paren :ws :word -1))}))

(defmacro first-where [pred ds]
  (with-syms [$pred $ds]
    ~(let [,$pred ,pred ,$ds ,ds]
       (var ret nil)
       (for i 0 (length ,$ds)
         (when (,$pred (,$ds i))
           (set ret (,$ds i))
           (break)))
       ret)))

(defn word-at :tested [location source]
  (let [{:character character-pos :line line-pos} location
        lines (string/split "\n" (code-mask source))]
    (if (or (< line-pos 0) (>= line-pos (length lines)))
      {:range [character-pos character-pos] :word ""}
      (let [line (lines line-pos)
            parsed (or (sort-by last (or (peg/match word-peg line) @[[0 "" 0]])))
            word (or (first-where |(>= ($ 2) character-pos) parsed) (last parsed))]
        {:range [(word 0) (word 2)] :word (word 1)}))))

(def sexp-peg
  (peg/compile
    ~{:s-exp (group (* (position) (* "(" (any (+ (drop :s-exp) (to (set "()")))) ")") (position)))
      :main (some (+ (if :s-exp 1) 1))}))

(defn sexp-at [location source]
  (let [{:character character-pos :line line-pos} location
        idx (+ character-pos (sum (map (comp inc length) (array/slice (string/split "\n" source) 0 line-pos))))
        s-exps (or (peg/match sexp-peg (code-mask source)) @[])]
    (if-let [sexp-range (last (filter |(< ($ 0) idx ($ 1)) s-exps))]
      {:source (string/slice source ;sexp-range) :range sexp-range}
      {:source "" :range @[line-pos character-pos]})))

(varfn to-index [location source]
  (let [{:character character-pos :line line-pos} location
        lines (string/split "\n" source)
        pre-lines (array/slice lines 0 line-pos)
        pre-lengths (map (comp inc length) pre-lines)
        pre-length (sum pre-lengths)]
    (comment prin "pre-lines: ") (comment pp pre-lines)
    (comment prin "pre-lengths: ") (comment pp pre-lengths)
    (comment prin "pre-length: ") (comment pp pre-length)
    (comment prin "character-pos: ") (comment pp character-pos)
    (+ character-pos pre-length)))

(defn from-index [index source]
  (def before (string/slice source 0 (min index (length source))))
  (def lines (string/split "\n" before))
  {:line (dec (length lines)) :character (length (last lines))})
