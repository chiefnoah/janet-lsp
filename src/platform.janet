(import spork/path)

(defn windows? [&opt kind]
  (= :windows (or kind (os/which))))

(defn path-list-separator [&opt kind]
  (if (windows? kind) ";" ":"))

(defn normalize-path [value &opt kind]
  (if (windows? kind) (string/ascii-lower value) value))

(defn temp-directory []
  (or (some |(and $ (not (empty? $)) $)
            [(os/getenv "TMPDIR") (os/getenv "TEMP") (os/getenv "TMP")])
      (if (windows?) "." "/tmp")))

(defn temp-path [name]
  (path/join (temp-directory) name))

(defn executable-names [name &opt kind pathext]
  (if (or (not (windows? kind)) (path/ext name))
    [name]
    (map |(string name (string/ascii-lower $))
         (or pathext [".EXE" ".CMD" ".BAT" ".COM"]))))

(defn find-executable [name]
  (def paths
    (string/split (path-list-separator) (or (os/getenv "PATH") "")))
  (def pathext
    (and (windows?)
         (string/split ";" (or (os/getenv "PATHEXT")
                               ".COM;.EXE;.BAT;.CMD"))))
  (first
    (catseq [directory :in paths
             candidate :in (executable-names name nil pathext)
             :let [filepath (path/join directory candidate)]
             :when (os/stat filepath)]
      (path/abspath filepath))))
