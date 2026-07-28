(import ./eval)
(import ./logging)
(import ./lint)
(import ./position)

(defn- code [message]
  (cond
    (string/has-prefix? "parse error:" message)
    (if (string/find "unexpected end of source" message)
      "janet.parse.unclosed-delimiter"
      "janet.parse")
    (string/has-prefix? "compile warning:" message) "janet.compile.warning"
    (string/has-prefix? "compile error:" message) "janet.compile"
    (string/has-prefix? "runtime error:" message) "janet.runtime"
    "janet.analysis"))

(defn run [filepath content encoding workspace &opt version]
  (let [items @[]
        [compiler-results env]
        (eval/eval-buffer content
                          (or filepath "untitled.janet")
                          {:trusted (workspace :trusted)
                           :base-env (workspace :env)
                           :unique-paths (workspace :unique-paths)})
        lint-results (if (> (length content) eval/max-source-bytes)
                       @[]
                       (lint/analyze content env))
        results (array ;compiler-results ;lint-results)]
    (logging/dbg (string/format "eval-buffer returned: %m" results) [:evaluation])
    (each result results
      (def diagnostic-code (get result :code))
      (match result
        {:location [line column] :message message :severity severity}
        (when-let [location
                   (position/byte->lsp-position
                     content
                     {:line (max 0 (dec line)) :character (max 0 (dec column))}
                     encoding)]
          (array/push items
                      {:range {:start location :end location}
                       :message message
                       :severity severity
                       :source "janet-lsp"
                       :code (or diagnostic-code (code message))
                       :data {:contentHash (hash content) :version version}}))))
    (logging/dbg (string/format "diagnostics: %m" items) [:evaluation])
    [items env]))
