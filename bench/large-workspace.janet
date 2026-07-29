(import ../src/index)

(def files (or (scan-number (get (dyn :args) 1)) 200))
(def definitions-per-file (or (scan-number (get (dyn :args) 2)) 100))
(def workspace @{:index @{}})
(def sources
  (map
    (fn [file]
      (string
        (if (= file 0) ""
          (string "(import ./module-" (dec file) " :as previous)\n"
                  "(previous/function-" (dec file) "-0)\n"))
        (string/join
          (map |(string "(defn function-" file "-" $ " [value] value)")
               (range definitions-per-file))
          "\n")))
    (range files)))
(def started (os/clock))

(for file 0 files
  (def uri (string "file:///benchmark/module-" file ".janet"))
  (put (workspace :index) uri (index/analyze uri (sources file))))

(def analyzed (os/clock))
(index/relink workspace)
(def linked (os/clock))
(printf "files=%d definitions=%d analyze=%.3fs relink=%.3fs total=%.3fs"
        files (* files definitions-per-file)
        (- analyzed started) (- linked analyzed) (- linked started))
