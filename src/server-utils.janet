(import ./position)
(import ./platform)
(import ./uri)

(defn document-uri [params]
  (get-in params ["textDocument" "uri"]))

(defn document [state params]
  (get-in state [:documents (document-uri params)]))

(defn path-in-workspace? [filepath root-path]
  (def candidate (platform/normalize-path filepath))
  (def root (platform/normalize-path root-path))
  (or (= candidate root)
      (string/has-prefix? (string root (if (string/has-suffix? "/" root) "" "/"))
                          candidate)))

(defn workspace-for-path [state filepath]
  (var owner nil)
  (when filepath
    (each workspace (values (state :workspaces))
      (when (and (path-in-workspace? filepath (workspace :path))
                 (or (nil? owner)
                     (> (length (workspace :path)) (length (owner :path)))))
        (set owner workspace))))
  (or owner (state :standalone-workspace)))

(defn document-workspace [state document]
  (workspace-for-path state (document :path)))

(defn request-byte-position [state params content]
  (position/lsp->byte-position content (get params "position")
                               (state :position-encoding)))

(defn lsp-range [state content range]
  {:start (position/byte->lsp-position content (range :start)
                                       (state :position-encoding))
   :end (position/byte->lsp-position content (range :end)
                                     (state :position-encoding))})

(defn content [state document-uri]
  (or (get-in state [:documents document-uri :content])
      (when-let [filepath (uri/file-uri->path document-uri)
                 found (os/stat filepath)]
        (try (slurp filepath) ([_] nil)))))

(defn same-position? [a b]
  (and a b (= (a :line) (b :line)) (= (a :character) (b :character))))

(defn position-in-range? [position range]
  (def start (or (get range "start") (get range :start)))
  (def end (or (get range "end") (get range :end)))
  (def line (or (get position "line") (get position :line)))
  (def character (or (get position "character") (get position :character)))
  (def start-line (and start (or (get start "line") (get start :line))))
  (def start-character
    (and start (or (get start "character") (get start :character))))
  (def end-line (and end (or (get end "line") (get end :line))))
  (def end-character
    (and end (or (get end "character") (get end :character))))
  (and (number? line) (number? character)
       (number? start-line) (number? start-character)
       (number? end-line) (number? end-character)
       (or (> line start-line)
           (and (= line start-line) (>= character start-character)))
       (or (< line end-line)
           (and (= line end-line) (<= character end-character)))))

(defn base-name [name]
  (last (string/split "/" name)))

(defn versioned-edit [state document-uri edits]
  {:textDocument {:uri document-uri
                  :version (get-in state [:documents document-uri :version])}
   :edits edits})
