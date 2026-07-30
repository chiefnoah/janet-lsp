(import ./diagnostics)
(import ./index)
(import ./parser)
(import ./semantic-tokens)
(import ./signatures)

(def max-snapshots 4)

(defn key [version content]
  (string (if (nil? version) "null" version) ":" (index/content-hash content)))

(defn- diagnostic-result-id [workspace snapshot-key items]
  (string (workspace :diagnostic-generation) ":"
          (or (workspace :index-generation) 0) ":" snapshot-key ":"
          (index/content-hash (string/format "%j" items))))

(defn build [document workspace encoding]
  (let [content (document :content)
        version (document :version)
        syntax-tree (try (parser/syntax-tree content) ([_] {:tag :top :value @[]}))
        record (index/analyze (document :uri) content syntax-tree)
        [items env]
        (diagnostics/run (document :path) content encoding workspace syntax-tree record
                         version)]
    (index/add-generated-to-record record (document :uri) (document :path) env)
    {:key (key version content)
     :version version
     :content-hash (index/content-hash content)
      :source content
     :workspace-uri (workspace :uri)
     :trusted (workspace :trusted)
     :diagnostic-generation (workspace :diagnostic-generation)
     :index-generation (or (workspace :index-generation) 0)
     :diagnostic-result-id (diagnostic-result-id workspace (key version content) items)
     :syntax-tree syntax-tree
     :signatures (signatures/all content)
     :diagnostics items
     :eval-env env
     :index record
     :references (record :references)
      :semantic (semantic-tokens/records record env content)}))

(defn current [document workspace]
  (def snapshot (document :analysis))
  (when (and snapshot
             (= (snapshot :key) (key (document :version) (document :content)))
             (= (snapshot :workspace-uri) (workspace :uri))
             (= (snapshot :trusted) (workspace :trusted))
             (= (snapshot :diagnostic-generation)
                (workspace :diagnostic-generation))
             (= (or (snapshot :index-generation) 0)
                (or (workspace :index-generation) 0)))
    snapshot))

(defn find-snapshot [document snapshot-key]
  (get-in document [:snapshots snapshot-key]))

(defn store [document snapshot]
  (unless (document :snapshots) (put document :snapshots @{}))
  (unless (document :snapshot-order) (put document :snapshot-order @[]))
  (unless (get (document :snapshots) (snapshot :key))
    (put (document :snapshots) (snapshot :key) snapshot)
    (array/push (document :snapshot-order) (snapshot :key)))
  (while (> (length (document :snapshot-order)) max-snapshots)
    (def oldest ((document :snapshot-order) 0))
    (array/remove (document :snapshot-order) 0)
    (put (document :snapshots) oldest nil))
  (put document :analysis snapshot)
  (put document :eval-env (snapshot :eval-env))
  snapshot)

(defn invalidate [document]
  (put document :analysis nil)
  (put document :snapshots @{})
  (put document :snapshot-order @[])
  document)

(defn replace-record [workspace document-uri record]
  (def previous (get-in workspace [:index document-uri]))
  (def same-content
    (and previous record
         (= (previous :content-hash) (record :content-hash))))
  (def previous-generated
    (and previous (filter |($ :generated) (previous :definitions))))
  (def generated
    (and record (filter |($ :generated) (record :definitions))))
  (unless (and same-content (deep= previous-generated generated))
    (put (workspace :index) document-uri record)
    (put workspace :links-dirty true))
  (unless same-content
    (put workspace :index-generation
         (inc (or (workspace :index-generation) 0))))
  (or (workspace :index-generation) 0))

(defn install [document workspace snapshot]
  (replace-record workspace (document :uri) (snapshot :index))
  (def linked-record (get-in workspace [:index (document :uri)]))
  (def installed
    (merge snapshot
            {:index linked-record
             :references (linked-record :references)
             :index-generation (or (workspace :index-generation) 0)
             :semantic (semantic-tokens/records
                          linked-record (snapshot :eval-env)
                          (snapshot :source) workspace)
            :diagnostic-result-id
            (diagnostic-result-id workspace (snapshot :key)
                                  (snapshot :diagnostics))}))
  (store document installed)
  installed)

(defn refresh [document workspace encoding]
  (install document workspace (build document workspace encoding)))
