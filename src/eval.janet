(import ./logging)

(varfn is-safe-def :private [])

(var- safe-forms {})

(defn- no-side-effects
  `Check if form may have side effects. If returns true, then the src
  must not have side effects, such as calling a C function.`
  [src]
  (cond
    (tuple? src) (cond
                   (= (tuple/type src) :brackets) (all no-side-effects src)
                   (= true (get safe-forms (src 0))) true
                   (get safe-forms (src 0)) ((get safe-forms (src 0)) src)
                   (get (dyn 'safe-forms) :flycheck))
    (array? src) (all no-side-effects src)
    (dictionary? src) (and (all no-side-effects (keys src))
                           (all no-side-effects (values src)))
    true))

(varfn is-safe-def :private [x]
  (or (filter |(= $ :flycheck) (tuple/slice x 2 -2))
      (no-side-effects (last x))))

(set safe-forms {'defn true 'varfn true 'defn- true 'defmacro true 'defmacro- true
                 'def is-safe-def 'var is-safe-def 'def- is-safe-def 'var- is-safe-def
                 'defglobal is-safe-def 'varglobal is-safe-def
                 #'merge-into true
                 'fn true
                 #'keyword true 'short-fn true
})

(def- importers {'import true 'import* true 'dofile true 'require true})

(defn- use-2 [evaluator args]
  (each a args (import* (string a) :prefix "" :evaluator evaluator)))

(defn- flycheck-evaluator
  ``An evaluator function that is passed to `run-context` that lints (flychecks) code.
  This means code will parsed and compiled, macros executed, but the code will not be run.
  Used by `flycheck`.``
  [thunk source env where]

  (when (tuple? source)
    (let [head (source 0)
          safe-check (or (safe-forms head)
                         (when (and (symbol? head) (string/has-prefix? "define-" head))
                           is-safe-def)
                         (get (dyn head) :flycheck))]
      (cond
        (= 'upscope head)
        (each f (tuple/slice source 1)
          (flycheck-evaluator (compile f) f env where))
        # Use
        (= 'use head) (use-2 flycheck-evaluator (tuple/slice source 1))
        # Import-like form
        (importers head)
        (if (or (string/has-prefix? "." (source 1))
                (string/has-prefix? "/" (source 1)))
          (let [[line column] (tuple/sourcemap source)
                newtup (tuple/setmap (tuple ;source :evaluator flycheck-evaluator) line column)]
            ((compile newtup env where)))
          (thunk))
        # Sometimes safe form
        (function? safe-check) (if (safe-check source) (thunk))
        # Always safe form
        safe-check (thunk)))))

(defn safe-buffer [str &opt filename base-env]
  (default filename "eval.janet")
  (def parse-state (parser/new))
  (parser/consume parse-state str)
  (parser/eof parse-state)
  (def diagnostics @[])
  (when (= :error (parser/status parse-state))
    (array/push diagnostics
                {:message (string/format "parse error: %s" (parser/error parse-state))
                 :location (parser/where parse-state)
                 :severity 1}))
  [diagnostics (make-env (or base-env root-env))])

(defn runtime-location [fib]
  (try
    (let [frame (first (debug/stack fib))
          slots (and frame (frame :slots))
          line (and (indexed? slots) (> (length slots) 2) (slots 1))
          column (and (indexed? slots) (> (length slots) 2) (slots 2))]
      (if (and (number? line) (number? column))
        [line column]
        [1 1]))
    ([_] [1 1])))

(defn eval-buffer [str &opt filename options]
  (logging/info (string/format "`eval-buffer` received filename: `%s`" (or filename "none")) [:evaluation])

  (default filename "eval.janet")
  (default options {})
  (def trusted (options :trusted))
  (def base-env (options :base-env))
  (def unique-paths (options :unique-paths))
  (def max-source-bytes (or (options :max-source-bytes) 1048576))
  (if (> (length str) max-source-bytes)
    [[{:message (string/format "analysis limit exceeded: source is larger than %d bytes"
                               max-source-bytes)
       :location [1 1]
       :severity 1}]
     (make-env (or base-env root-env))]
  (if (not trusted)
    (safe-buffer str filename base-env)
    (do
  (var state (string str))
  (defn chunks [buf parser]
    (def ret state)
    (set state nil)
    (when ret
      (buffer/push-string buf str)
      (buffer/push-string buf "\n")))

  (def fresh-env (make-env (or base-env root-env)))

  (each path (or unique-paths @[])
    (cond
      (string/has-suffix? ".janet" path) (array/push ((fresh-env 'module/paths) :value) [path :source])
      (string/has-suffix? ".so" path) (array/push ((fresh-env 'module/paths) :value) [path :native])
      (string/has-suffix? ".jimage" path) (array/push ((fresh-env 'module/paths) :value) [path :jimage])))

  (def eval-fiber
    (fiber/new
      |(do (var returnval @[])
         (try (run-context {:chunks chunks
                            :on-compile-error (fn compile-error [msg errf where line col]
                                                (array/push returnval {:message (string/format "compile error: %s" msg)
                                                                       :location [line col]
                                                                       :severity 1}))
                            :on-compile-warning (fn compile-warning [msg errf where line col]
                                                  (array/push returnval {:message (string/format "compile warning: %s" msg)
                                                                         :location [line col]
                                                                         :severity 2}))
                            :on-parse-error (fn parse-error [p x]
                                              (array/push returnval {:message (string/format "parse error: %s" (parser/error p))
                                                                     :location (parser/where p)
                                                                     :severity 1}))
                            :evaluator flycheck-evaluator
                            :fiber-flags :i
                            :source filename})
           ([err fib]
             (array/push returnval {:message (string/format "runtime error: %s" err)
                                    :location (runtime-location fib)
                                    :severity 1})))
         returnval) :e fresh-env))
  (fiber/setmaxstack eval-fiber 2048)
  (def eval-fiber-return (resume eval-fiber))
  (logging/dbg (string/format "`eval-buffer` is returning: %m" eval-fiber-return) [:evaluation])
  [eval-fiber-return fresh-env]))))
