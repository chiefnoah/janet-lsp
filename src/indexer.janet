(import ./index)

(defn main [& args]
  (def root (args 1))
  (def output (args 2))
  (def exclusions (parse (args 3)))
  (def temporary (string output ".tmp"))
  (spit temporary (string/format "%j" (index/scan root exclusions)))
  (os/rename temporary output))
