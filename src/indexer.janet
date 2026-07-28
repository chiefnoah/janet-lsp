(import ./index-cache)

(defn main [& args]
  (def root (args 1))
  (def output (args 2))
  (def exclusions (parse (args 3)))
  (def cache-path (args 4))
  (def root-uri (args 5))
  (def temporary (string output ".tmp"))
  (def result
    (try {:ok true :index (index-cache/rebuild cache-path root-uri root exclusions false)}
      ([err] {:ok false :error (string err)})))
  (spit temporary (string/format "%j" result))
  (os/rename temporary output))
