(import ./position)
(import ./uri)

(defn document-uri [params]
  (get-in params ["textDocument" "uri"]))

(defn document [state params]
  (get-in state [:documents (document-uri params)]))

(defn path-in-workspace? [filepath root-path]
  (def candidate (if (= :windows (os/which)) (string/ascii-lower filepath) filepath))
  (def root (if (= :windows (os/which)) (string/ascii-lower root-path) root-path))
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
  (def start (get range "start"))
  (def end (get range "end"))
  (and (or (> (position :line) (get start "line"))
           (and (= (position :line) (get start "line"))
                (>= (position :character) (get start "character"))))
       (or (< (position :line) (get end "line"))
           (and (= (position :line) (get end "line"))
                (<= (position :character) (get end "character"))))))

(defn base-name [name]
  (last (string/split "/" name)))

(defn versioned-edit [state document-uri edits]
  {:textDocument {:uri document-uri
                  :version (get-in state [:documents document-uri :version])}
   :edits edits})
