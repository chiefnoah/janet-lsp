(def version "0.0.12")

(def semantic-token-types
  ["namespace" "type" "function" "macro" "variable" "parameter"
   "keyword" "string" "number" "comment" "operator"])

(def semantic-token-modifiers
  ["declaration" "definition" "readonly" "defaultLibrary"])

(def commit
  (with [proc (os/spawn ["git" "rev-parse" "--short" "HEAD"] :xp {:out :pipe})]
    (let [[out] (ev/gather
                  (ev/read (proc :out) :all)
                  (os/proc-wait proc))]
      (if out (string/trimr out) ""))))

(defn server-info []
  {:name "janet-lsp" :version version :commit commit})
