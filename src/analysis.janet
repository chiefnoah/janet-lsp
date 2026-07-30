(import ./diagnostics)
(import ./eval)
(import ./index)
(import ./parser)
(import ./request-control)
(import ./semantic-tokens)
(import ./signatures)

(def max-snapshots 4)

(defn key [version content]
  (string (if (nil? version) "null" version) ":" (index/content-hash content)))

(defn- diagnostic-result-id [workspace snapshot-key items]
  (string (workspace :diagnostic-generation) ":"
          (or (workspace :index-generation) 0) ":" snapshot-key ":"
          (index/content-hash (string/format "%j" items))))

(defn build [document workspace encoding &opt state request-id]
  (let [content (document :content)
        version (document :version)
        oversized (> (length content) eval/max-source-bytes)
        _ (when state (request-control/checkpoint state request-id))
        syntax-tree (if oversized
                      {:tag :top :value @[]}
                      (try (parser/syntax-tree content)
                        ([_] {:tag :top :value @[]})))
        _ (when state (request-control/checkpoint state request-id))
        raw-record (index/analyze (document :uri) (if oversized "" content) syntax-tree)
        record (if oversized
                 (merge raw-record {:content-hash (index/content-hash content)})
                 raw-record)
        _ (when state (request-control/checkpoint state request-id))
        [items env]
        (diagnostics/run (document :path) content encoding workspace syntax-tree record
                         version state request-id)]
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
      :signatures (if oversized @[] (signatures/all content))
     :diagnostics items
     :eval-env env
     :index record
      :references (record :references)}))

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
  (put (document :snapshots) (snapshot :key) snapshot)
  (unless (has-value? (document :snapshot-order) (snapshot :key))
    (array/push (document :snapshot-order) (snapshot :key)))
  (while (> (length (document :snapshot-order)) max-snapshots)
    (def oldest ((document :snapshot-order) 0))
    (array/remove (document :snapshot-order) 0)
    (put (document :snapshots) oldest nil))
  (put document :analysis snapshot)
  (put document :eval-env (snapshot :eval-env))
  snapshot)

(defn note-version [document]
  (unless (document :snapshot-order) (put document :snapshot-order @[]))
  (def snapshot-key (key (document :version) (document :content)))
  (unless (has-value? (document :snapshot-order) snapshot-key)
    (array/push (document :snapshot-order) snapshot-key))
  (while (> (length (document :snapshot-order)) max-snapshots)
    (def oldest ((document :snapshot-order) 0))
    (array/remove (document :snapshot-order) 0)
    (put (document :snapshots) oldest nil))
  document)

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

(defn install [document workspace snapshot &opt state request-id]
  (replace-record workspace (document :uri) (snapshot :index))
  (def linked-record (get-in workspace [:index (document :uri)]))
  (def installed
    (merge snapshot
            {:index linked-record
             :references (linked-record :references)
             :index-generation (or (workspace :index-generation) 0)
              :semantic (if (> (length (snapshot :source)) eval/max-source-bytes)
                          @[]
                          (semantic-tokens/records
                            linked-record (snapshot :eval-env)
                            (snapshot :source) workspace state request-id))
            :diagnostic-result-id
            (diagnostic-result-id workspace (snapshot :key)
                                  (snapshot :diagnostics))}))
  (store document installed)
  installed)

(defn refresh [document workspace encoding &opt state request-id]
  (install document workspace (build document workspace encoding state request-id)
           state request-id))
