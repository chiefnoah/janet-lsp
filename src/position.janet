(defn- continuation? [byte]
  (= 128 (band byte 192)))

(defn- decode-codepoint [bytes index]
  (def lead (bytes index))
  (def width
    (cond
      (< lead 128) 1
      (<= 194 lead 223) 2
      (<= 224 lead 239) 3
      (<= 240 lead 244) 4
      nil))
  (unless (and width (<= (+ index width) (length bytes)))
    (error "invalid UTF-8 in document"))
  (for i 1 width
    (unless (continuation? (bytes (+ index i)))
      (error "invalid UTF-8 in document")))
  (def codepoint
    (case width
      1 lead
      2 (+ (blshift (band lead 31) 6)
           (band (bytes (inc index)) 63))
      3 (+ (blshift (band lead 15) 12)
           (blshift (band (bytes (inc index)) 63) 6)
           (band (bytes (+ index 2)) 63))
      4 (+ (blshift (band lead 7) 18)
           (blshift (band (bytes (inc index)) 63) 12)
           (blshift (band (bytes (+ index 2)) 63) 6)
           (band (bytes (+ index 3)) 63))))
  (when (or (and (= width 3) (< codepoint 0x800))
            (and (= width 4) (< codepoint 0x10000))
            (<= 0xD800 codepoint 0xDFFF)
            (> codepoint 0x10FFFF))
    (error "invalid UTF-8 in document"))
  [codepoint width])

(defn- code-units [codepoint width encoding]
  (case encoding
    "utf-8" width
    "utf-32" 1
    (if (> codepoint 0xFFFF) 2 1)))

(defn- nonnegative-integer? [value]
  (and (number? value) (= value (math/floor value)) (>= value 0)))

(defn units-to-byte [line character encoding]
  (if (not (nonnegative-integer? character))
    nil
    (let [bytes (string/bytes line)]
      (var byte-index 0)
      (var units 0)
      (var result nil)
      (var done false)
      (while (and (not done) (< byte-index (length bytes)))
        (if (= units character)
          (do
            (set result byte-index)
            (set done true))
          (let [[codepoint width] (decode-codepoint bytes byte-index)]
            (+= units (code-units codepoint width encoding))
            (if (> units character)
              (set done true)
              (+= byte-index width)))))
      (if done result
        (if (= units character) byte-index nil)))))

(defn byte-to-units [line byte-column encoding]
  (if (not (nonnegative-integer? byte-column))
    nil
    (let [bytes (string/bytes line)]
      (if (> byte-column (length bytes))
        nil
        (do
          (var byte-index 0)
          (var units 0)
          (var valid true)
          (while (and valid (< byte-index byte-column))
            (def [codepoint width] (decode-codepoint bytes byte-index))
            (if (> (+ byte-index width) byte-column)
              (set valid false)
              (do
                (+= units (code-units codepoint width encoding))
                (+= byte-index width))))
          (if valid units nil))))))

(defn lsp->byte-position [source lsp-position encoding]
  (def line-number (or (get lsp-position :line) (get lsp-position "line")))
  (def character (or (get lsp-position :character) (get lsp-position "character")))
  (def lines (string/split "\n" source))
  (when (and (nonnegative-integer? line-number) (< line-number (length lines)))
    (when-let [byte-column (units-to-byte (lines line-number) character encoding)]
      {:line line-number :character byte-column})))

(defn byte->lsp-position [source byte-position encoding]
  (def line-number (or (get byte-position :line) (get byte-position "line")))
  (def byte-column (or (get byte-position :character) (get byte-position "character")))
  (def lines (string/split "\n" source))
  (when (and (nonnegative-integer? line-number) (< line-number (length lines)))
    (when-let [character (byte-to-units (lines line-number) byte-column encoding)]
      {:line line-number :character character})))

(defn document-end [source encoding]
  (def lines (string/split "\n" source))
  (def line-number (dec (length lines)))
  (byte->lsp-position source
                      {:line line-number :character (length (lines line-number))}
                      encoding))
