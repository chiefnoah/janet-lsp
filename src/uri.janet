(import spork/path)

(defn- hex-value [byte]
  (cond
    (<= 48 byte 57) (- byte 48)
    (<= 65 byte 70) (+ 10 (- byte 65))
    (<= 97 byte 102) (+ 10 (- byte 97))
    nil))

(defn percent-decode [value]
  (def bytes (string/bytes value))
  (def decoded @"")
  (var i 0)
  (while (< i (length bytes))
    (if (= 37 (bytes i))
      (do
        (when (>= (+ i 2) (length bytes))
          (error "invalid percent escape in URI"))
        (def high (hex-value (bytes (inc i))))
        (def low (hex-value (bytes (+ i 2))))
        (when (or (nil? high) (nil? low))
          (error "invalid percent escape in URI"))
        (buffer/push-byte decoded (+ (* high 16) low))
        (+= i 3))
      (do
        (buffer/push-byte decoded (bytes i))
        (+= i 1))))
  (string decoded))

(defn- unreserved-byte? [byte]
  (or (<= 48 byte 57)
      (<= 65 byte 90)
      (<= 97 byte 122)
      (has-value? [45 46 95 126] byte)))

(defn percent-encode-path [value]
  (def encoded @"")
  (each byte (string/bytes value)
    (if (or (unreserved-byte? byte) (= byte 47) (= byte 58))
      (buffer/push-byte encoded byte)
      (buffer/push-string encoded (string/format "%%%02X" byte))))
  (string encoded))

(defn file-uri->path [value]
  (if (not (and (string? value) (string/has-prefix? "file:" value)))
    nil
    (let [remainder (string/slice value 5)]
      (if (or (string/find "?" remainder) (string/find "#" remainder))
        nil
        (do
          (var authority "")
          (var uri-path remainder)
          (when (string/has-prefix? "//" remainder)
            (def authority-and-path (string/slice remainder 2))
            (if-let [slash (string/find "/" authority-and-path)]
              (do
                (set authority (string/slice authority-and-path 0 slash))
                (set uri-path (string/slice authority-and-path slash)))
              (do
                (set authority authority-and-path)
                (set uri-path ""))))
          (def decoded-path (percent-decode uri-path))
          (cond
            (and (not (empty? authority)) (not= authority "localhost"))
            (string "//" (percent-decode authority) decoded-path)

            (and (>= (length decoded-path) 3)
                 (= 47 (decoded-path 0))
                 (= 58 (decoded-path 2))
                 (or (<= 65 (decoded-path 1) 90)
                     (<= 97 (decoded-path 1) 122)))
            (string/slice decoded-path 1)

            decoded-path))))))

(defn path->file-uri [value]
  (def normalized (string/replace-all "\\" "/" value))
  (cond
    (string/has-prefix? "//" normalized)
    (string "file:" (percent-encode-path normalized))

    (and (>= (length normalized) 2)
         (= 58 (normalized 1))
         (or (<= 65 (normalized 0) 90)
             (<= 97 (normalized 0) 122)))
    (string "file:///" (percent-encode-path normalized))

    (string/has-prefix? "/" normalized)
    (string "file://" (percent-encode-path normalized))

    (path->file-uri (path/abspath normalized))))
