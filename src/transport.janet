(defn parse-headers [lines]
  (var content-length nil)
  (each raw-line lines
    (def line (string/trim raw-line))
    (unless (empty? line)
      (def separator (string/find ":" line))
      (unless separator
        (error (string "malformed LSP header: " line)))
      (def name (string/ascii-lower (string/trim (string/slice line 0 separator))))
      (def value (string/trim (string/slice line (inc separator))))
      (when (empty? name)
        (error "malformed LSP header: empty name"))
      (when (= name "content-length")
        (when content-length
          (error "malformed LSP headers: duplicate Content-Length"))
        (def digits (first (peg/match ~(* (<- :d+) -1) value)))
        (unless digits
          (error "malformed LSP headers: invalid Content-Length"))
        (set content-length (scan-number digits)))))
  (unless content-length
    (error "malformed LSP headers: missing Content-Length"))
  content-length)

(defn read-exactly [file byte-count]
  (def body @"")
  (while (< (length body) byte-count)
    (def chunk (file/read file (- byte-count (length body))))
    (when (or (nil? chunk) (empty? chunk))
      (error (string/format "truncated LSP body: expected %d bytes, received %d"
                            byte-count (length body))))
    (buffer/push-string body chunk))
  body)

(defn read-frame [file]
  (def headers @[])
  (var reading true)
  (var eof false)
  (while reading
    (def line (file/read file :line))
    (when (or (nil? line) (empty? line))
      (if (empty? headers)
        (do
          (set eof true)
          (set reading false))
        (error "truncated LSP headers")))
    (unless eof
      (if (empty? (string/trim line))
        (set reading false)
        (array/push headers line))))
  (unless eof
    (read-exactly file (parse-headers headers))))

(defn write-frame [file body]
  (file/write file "Content-Length: " (string (length body)) "\r\n\r\n" body)
  (file/flush file))
