(import ./diagnostics)
(import ./index)
(import ./parser)
(import ./signatures)

(def max-snapshots 4)

(defn key [version content]
  (string (if (nil? version) "null" version) ":" (hash content)))

(defn- semantic-records [record env]
  (def definitions (record :definitions))
  (def records @[])
  (each definition definitions
    (array/push records {:range (definition :selection-range)
                         :type (if (= 12 (definition :kind)) 2 4)
                         :modifiers 3})
    (each parameter (definition :children)
      (array/push records {:range (parameter :selection-range)
                           :type 5 :modifiers 1})))
  (each reference (record :references)
    (def range (reference :range))
    (unless (any? (map |(deep= range ($ :range)) records))
      (def name (reference :name))
      (def definition (first (filter |(= name ($ :name)) definitions)))
      (def binding (get env (symbol name) nil))
      (array/push records
                  {:range range
                   :type (cond
                           (string/has-prefix? ":" name) 6
                           (scan-number name) 8
                           (string/find "/" name) 0
                           definition (if (= 12 (definition :kind)) 2 4)
                           (and binding (binding :macro)) 3
                           (and binding
                                (has-value? [:function :cfunction]
                                            (type (binding :value)))) 2
                           4)
                   :modifiers 0})))
  (sort-by |[(get-in $ [:range :start :line])
             (get-in $ [:range :start :character])]
           records))

(defn build [document workspace encoding]
  (let [content (document :content)
        version (document :version)
        [items env]
        (diagnostics/run (document :path) content encoding workspace version)
        syntax-tree (try (parser/syntax-tree content) ([_] {:tag :top :value @[]}))
        record (index/analyze (document :uri) content syntax-tree)]
    (index/add-generated-to-record record (document :uri) (document :path) env)
    {:key (key version content)
     :version version
     :content-hash (hash content)
     :workspace-uri (workspace :uri)
     :trusted (workspace :trusted)
     :syntax-tree syntax-tree
     :signatures (signatures/all content)
     :diagnostics items
     :eval-env env
     :index record
     :references (record :references)
     :semantic (semantic-records record env)}))

(defn current [document workspace]
  (def snapshot (document :analysis))
  (when (and snapshot
             (= (snapshot :key) (key (document :version) (document :content)))
             (= (snapshot :workspace-uri) (workspace :uri))
             (= (snapshot :trusted) (workspace :trusted)))
    snapshot))

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

(defn install [document workspace snapshot]
  (store document snapshot)
  (index/update-record workspace (document :uri) (snapshot :index))
  snapshot)

(defn refresh [document workspace encoding]
  (install document workspace (build document workspace encoding)))
