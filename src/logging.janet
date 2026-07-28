(import spork/rpc)

(def log-file "janetlsp.log")

(defn debug-port [opts]
  (string (or (and opts (opts :debug-port)) 8037)))

(defn- rotate-log []
  (when (and (os/stat log-file)
             (> (get (os/stat log-file) :size) 5000000))
    (def oldest (string log-file ".5"))
    (when (os/stat oldest) (os/rm oldest))
    (var generation 4)
    (while (> generation 0)
      (def source (string log-file "." generation))
      (when (os/stat source)
        (os/rename source (string log-file "." (inc generation))))
      (-= generation 1))
    (os/rename log-file (string log-file ".1"))))

(defn log [output categories &opt level]
  (when (dyn :debug)
    (unless (dyn :client)
      (setdyn :client
              (try (rpc/client "127.0.0.1" (debug-port (dyn :opts)))
                ([_] false))))
    (when (dyn :client) (:print (dyn :client) output))
    (when (or (nil? level) (<= level (dyn :log-to-file-level)))
      (try
        (do
          (unless (os/stat log-file) (spit log-file ""))
          (rotate-log)
          (spit log-file (string output "\n") :a))
        ([err]
          (file/write stderr
                      (string/format "error while writing log file: %q\n" err)))))
    (when (and (or (empty? (dyn :log-categories))
                   (empty? categories)
                   (any? (map |(has-value? (dyn :log-categories) $) categories)))
               (or (nil? level) (<= level (dyn :log-level))))
      (file/write stderr (string output "\n"))))
  nil)

(defn emit [tag default-level value categories &opt level id]
  (default level default-level)
  (log (string/format "[%s%s:%s] %s"
                      tag
                      (if id (string ":" id) "")
                      (first categories)
                      (case (type value)
                        :string value
                        (string/format "%m" value)))
       categories level))

(defmacro dbg [output categories &opt level id]
  ~(when (dyn :debug) (,emit "DEBUG" 3 ,output ,categories ,level ,id)))

(defmacro info [output categories &opt level id]
  ~(when (dyn :debug) (,emit "INFO" 2 ,output ,categories ,level ,id)))

(defmacro message [output categories &opt level id]
  ~(when (dyn :debug) (,emit "MESSAGE" 2 ,output ,categories ,level ,id)))

(defmacro warn [output categories &opt level id]
  ~(when (dyn :debug) (,emit "WARNING" 1 ,output ,categories ,level ,id)))

(defmacro err [output categories &opt level id]
  ~(when (dyn :debug) (,emit "ERROR" 0 ,output ,categories ,level ,id)))

(defmacro fatal [output categories &opt level id]
  ~(when (dyn :debug) (,emit "FATAL" 0 ,output ,categories ,level ,id)))

(defmacro unknown [output categories &opt level id]
  ~(when (dyn :debug) (,emit "UNKNOWN" 0 ,output ,categories ,level ,id)))
