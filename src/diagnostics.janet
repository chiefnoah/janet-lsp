(import ./configuration)
(import ./eval)
(import ./index)
(import ./logging)
(import ./lint)
(import ./position)
(import ./request-control)
(import ./signatures)
(import ./static-diagnostics)

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

(defn run [filepath content encoding workspace tree record &opt version state request-id]
  (let [items @[]
        [compiler-results env]
        (eval/eval-buffer content
                          (or filepath "untitled.janet")
                          {:trusted (workspace :trusted)
                           :base-env (workspace :env)
                           :unique-paths (workspace :unique-paths)})
        _ (when state (request-control/checkpoint state request-id))
        lint-results (if (> (length content) eval/max-source-bytes)
                        @[]
                        (lint/analyze content env))
        _ (when state (request-control/checkpoint state request-id))
        call-results (if (> (length content) eval/max-source-bytes)
                        @[]
                        (signatures/diagnostics content record))
        _ (when state (request-control/checkpoint state request-id))
        static-results (if (> (length content) eval/max-source-bytes)
                         @[]
                         (static-diagnostics/analyze content tree record workspace env
                                                     (not (workspace :trusted))))
        raw-results
        (map (fn [result]
               (if (result :code)
                 result
                 (merge result {:code (code (or (result :message) ""))})))
             (array ;compiler-results ;lint-results ;call-results ;static-results))
        results (keep |(configuration/apply-severity $
                                                       (workspace :diagnostic-settings))
                      (configuration/suppress raw-results content))]
    (logging/dbg (string/format "eval-buffer returned: %m" results) [:evaluation])
    (each result results
      (def diagnostic-code (get result :code))
      (when-let [message (get result :message)
                 severity (get result :severity)]
        (def range
          (if-let [byte-range (get result :range)
                   start (position/byte->lsp-position content (byte-range :start)
                                                        encoding)
                   end (position/byte->lsp-position content (byte-range :end)
                                                      encoding)]
            {:start start :end end}
            (when-let [[line column] (get result :location)
                       location
                       (position/byte->lsp-position
                         content
                         {:line (max 0 (dec line)) :character (max 0 (dec column))}
                         encoding)]
              {:start location :end location})))
        (when range
          (array/push items
                      {:range range
                       :message message
                       :severity severity
                       :source "janet-lsp"
                       :code (or diagnostic-code (code message))
                       :data (merge (or (get result :data) {})
                                    {:contentHash (index/content-hash content)
                                     :version version})}))))
    (logging/dbg (string/format "diagnostics: %m" items) [:evaluation])
    [items env]))
