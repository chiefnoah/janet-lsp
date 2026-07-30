(import ./logging)

(def special-forms
  '[break def do fn if quasiquote quote set splice unquote upscope var while])

(defn make-module-entry [binding]
  (let [binding-type
        (cond
          (binding :redef) (type (in (binding :ref) 0))
          (binding :ref) (string :var " (" (type (in (binding :ref) 0)) ")")
          (binding :macro) :macro
          (binding :module) (string :module " (" (binding :kind) ")")
          (type (binding :value)))
        source-map (binding :source-map)
        documentation (binding :doc)]
    (string binding-type
            (when-let [[filepath line column] source-map]
              (string "  \n" filepath
                      (when line (string " on line " line))
                      (when column (string ", column " column))))
            "\n\n"
            (if documentation
              (let [parts (string/split "\n" documentation)]
                (if (and (string/has-prefix? "(" (parts 0))
                         (string/has-suffix? ")" (parts 0)))
                  (string/join (-> parts
                                   (array/insert 1 "```")
                                   (array/insert 0 "```janet"))
                               "\n")
                  documentation))
              "No documentation found.\n"))))

(defn make-special-form-entry [symbol]
  (string "special form\n\n(" symbol " ...)\n\n"
          "See https://janet-lang.org/docs/specials.html"))

(defn make-definition-entry [definition]
  (def form (definition :form))
  (def kind
    (cond
      (string/has-prefix? "defmacro" form) "macro"
      (has-value? ["defn" "defn-" "varfn" "varfn-"] form) "function"
      "definition"))
  (def parameters (map |($ :name) (get definition :children @[])))
  (def signature
    (when (not (empty? parameters))
      (string "```janet\n(" (definition :name) " ["
              (string/join parameters " ") "])\n```\n\n")))
  (def metadata @[])
  (when-let [target (get-in definition [:type-target :name])]
    (array/push metadata (string "Type: `" target "`")))
  (when-let [target (definition :return-target)]
    (array/push metadata (string "Returns: `" target "`")))
  (each target (get definition :implementation-targets @[])
    (array/push metadata (string "Implements: `" (target :name) "`")))
  (string kind "  \n" (definition :uri) " on line "
          (inc (get-in definition [:selection-range :start :line])) "\n\n"
          signature
          (or (definition :doc) "No documentation found.\n")
          (when (not (empty? metadata))
            (string "\n\n" (string/join metadata "  \n")))))

(defn get-signature [symbol env]
  (assert env "get-signature: env is nil")
  (if-let [documentation (get-in env [symbol :doc])]
    (first (string/split "\n" documentation))
    (when (has-value? special-forms symbol)
      (string "(" symbol " ... )"))))

(defn my-doc* [symbol env]
  (assert env "my-doc*: env is nil")
  (cond
    (env symbol) (make-module-entry (env symbol))
    (has-value? special-forms symbol) (make-special-form-entry symbol)
    (let [find-fiber (fiber/new |(module/find (string symbol)) :e env)
          found (resume find-fiber)]
      (unless (= :error (fiber/status find-fiber))
        (def [fullpath module-kind] found)
        (def cache-fiber (fiber/new |(in module/cache fullpath) :e env))
        (def cached (resume cache-fiber))
        (cond
          (= :error (fiber/status cache-fiber))
          (logging/err (string/format "symbol %m not found" symbol) [:hover])
          cached
          (make-module-entry {:module true
                              :kind module-kind
                              :source-map [fullpath nil nil]
                              :doc (in cached :doc)}))))))
