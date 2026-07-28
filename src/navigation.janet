(import ./index)
(import ./lookup)
(import ./parser)
(import ./position)
(import ./server-utils)
(import ./uri)
(import spork/path)

(defn- symbol-context [state params]
  (def document-uri (server-utils/document-uri params))
  (def document (server-utils/document state params))
  (def content (document :content))
  (def location (server-utils/request-byte-position state params content))
  (def word (lookup/word-at location content))
  (def name (word :word))
  (def workspace (server-utils/document-workspace state document))
  (def local (and (not (empty? name))
                  (parser/definition-at location content name)))
  {:uri document-uri
   :document document
   :content content
   :location location
   :word word
   :name name
   :workspace workspace
   :local local
   :indexed (and (not local) (not (empty? name))
                 (first (index/definitions workspace (server-utils/base-name name))))})

(defn on-definition [state params]
  (def context (symbol-context state params))
  (def name (context :name))
  (cond
    (context :local)
    [:ok state {:uri (context :uri)
                :range (server-utils/lsp-range state (context :content)
                                               (get-in context [:local :range]))}]

    (context :indexed)
    (let [definition (context :indexed)
          target-uri (definition :uri)
          target-content (server-utils/content state target-uri)]
      [:ok state
       (if target-content
         {:uri target-uri
          :range (server-utils/lsp-range state target-content (definition :range))}
         :null)])

    (not (empty? name))
    (if-let [binding (get-in context [:document :eval-env (symbol name)])
             [source-path source-line source-column] (binding :source-map)
             target-path (path/abspath source-path)
             found (os/stat target-path)
             target-content (slurp target-path)
             target-position
             (position/byte->lsp-position
               target-content
               {:line (max 0 (dec source-line))
                :character (max 0 (dec source-column))}
               (state :position-encoding))]
      [:ok state {:uri (uri/path->file-uri target-path)
                  :range {:start target-position :end target-position}}]
      [:ok state :null])

    [:ok state :null]))

(defn- document-symbol [state content definition]
  {:name (definition :name)
   :kind (definition :kind)
   :range (server-utils/lsp-range state content (definition :range))
   :selectionRange (server-utils/lsp-range state content
                                            (definition :selection-range))
   :children (map |{:name ($ :name)
                    :kind ($ :kind)
                    :range (server-utils/lsp-range state content ($ :range))
                    :selectionRange (server-utils/lsp-range
                                      state content ($ :selection-range))}
                  (definition :children))})

(defn on-document-symbols [state params]
  (if-let [document (server-utils/document state params)]
    (let [record (index/analyze (document :uri) (document :content))]
      [:ok state (map |(document-symbol state (document :content) $)
                      (record :definitions))])
    [:ok state @[]]))

(defn on-workspace-symbols [state params]
  (def query (string/ascii-lower (or (get params "query") "")))
  (def symbols @[])
  (each workspace (values (state :workspaces))
    (each definition (index/definitions workspace)
      (when (string/find query (string/ascii-lower (definition :name)))
        (when-let [content (server-utils/content state (definition :uri))]
          (array/push symbols
                      {:name (definition :name)
                       :kind (definition :kind)
                       :location {:uri (definition :uri)
                                  :range (server-utils/lsp-range
                                           state content
                                           (definition :selection-range))}})))))
  [:ok state symbols])

(defn- raw-references [context include-declaration]
  (var locations @[])
  (if (context :local)
    (each reference (parser/references-for (context :name) (context :content))
      (def reference-start (get-in reference [:range :start]))
      (def resolved (parser/definition-at reference-start
                                          (context :content) (context :name)))
      (when (or (server-utils/same-position?
                  reference-start (get-in context [:local :range :start]))
                (server-utils/same-position?
                  (get-in resolved [:range :start])
                  (get-in context [:local :range :start])))
        (array/push locations {:uri (context :uri) :range (reference :range)})))
    (each record (values ((context :workspace) :index))
      (each reference (record :references)
        (def base-name (server-utils/base-name (context :name)))
        (when (or (= base-name (reference :name))
                  (string/has-suffix? (string "/" base-name) (reference :name)))
          (array/push locations {:uri (reference :uri) :range (reference :range)})))))
  (unless include-declaration
    (def definition (or (context :local) (context :indexed)))
    (def definition-uri (if (context :local) (context :uri)
                          (and definition (definition :uri))))
    (def definition-position
      (get-in definition [(if (context :local) :range :selection-range) :start]))
    (set locations
         (filter |(not (and (= definition-uri ($ :uri))
                            (server-utils/same-position?
                              definition-position (get-in $ [:range :start]))))
                 locations)))
  locations)

(defn references [state params]
  (def context (symbol-context state params))
  (if (empty? (context :name))
    @[]
    (distinct
      (catseq [found :in (raw-references
                           context
                           (not= false (get-in params ["context" "includeDeclaration"])))
               :let [content (server-utils/content state (found :uri))]
               :when content]
        {:uri (found :uri)
         :range (server-utils/lsp-range state content (found :range))}))))

(defn on-references [state params]
  [:ok state (references state params)])

(defn- rename-target [state params]
  (def context (symbol-context state params))
  (when (or (context :local) (context :indexed))
    {:name (context :name)
     :content (context :content)
     :range {:start {:line (get-in context [:location :line])
                     :character (first (get-in context [:word :range]))}
             :end {:line (get-in context [:location :line])
                   :character (last (get-in context [:word :range]))}}}))

(defn- valid-symbol? [name]
  (try (let [parsed (parse name)]
         (and (symbol? parsed) (= name (string parsed))))
    ([_] false)))

(defn on-prepare-rename [state params]
  (if-let [target (rename-target state params)]
    [:ok state {:range (server-utils/lsp-range state (target :content) (target :range))
                :placeholder (target :name)}]
    [:rpc-error state -32602 "Invalid params" "symbol cannot be renamed"]))

(defn on-rename [state params]
  (def new-name (get params "newName"))
  (cond
    (not (valid-symbol? new-name))
    [:rpc-error state -32602 "Invalid params" "newName must be one Janet symbol"]

    (nil? (rename-target state params))
    [:rpc-error state -32602 "Invalid params" "symbol cannot be renamed"]

    (let [changes @{}
          references (references state
                                 {"textDocument" (get params "textDocument")
                                  "position" (get params "position")
                                  "context" {"includeDeclaration" true}})]
      (each reference references
        (def document-uri (reference :uri))
        (def content (server-utils/content state document-uri))
        (def range (reference :range))
        (def line ((string/split "\n" content) (get-in range [:start :line])))
        (def start (position/units-to-byte line (get-in range [:start :character])
                                           (state :position-encoding)))
        (def end (position/units-to-byte line (get-in range [:end :character])
                                         (state :position-encoding)))
        (def old-name (string/slice line start end))
        (def slash (string/find "/" old-name))
        (def replacement (if slash
                           (string (string/slice old-name 0 (inc slash)) new-name)
                           new-name))
        (unless (get changes document-uri) (put changes document-uri @[]))
        (array/push (get changes document-uri) {:range range :newText replacement}))
      [:ok state
       {:documentChanges
        (seq [[document-uri edits] :pairs changes]
          (server-utils/versioned-edit state document-uri edits))}])))
