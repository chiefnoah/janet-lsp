(import ./index)

(defn main [& args]
  (def root (args 1))
  (def output (args 2))
  (def exclusions (parse (args 3)))
  (def temporary (string output ".tmp"))
  (def result
    (try {:ok true :index (index/scan root exclusions)}
      ([err] {:ok false :error (string err)})))
  (spit temporary (string/format "%j" result))
  (os/rename temporary output))
