(defn concat-dedup-by-label [& arrays]
  (let [seen @{}
        result @[]]
    (each arr arrays
      (each item arr
        (let [item-label (get item :label)]
          (unless (seen item-label)
            (put seen item-label true)
            (array/push result item)))))
    result))
