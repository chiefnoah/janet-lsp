(import ./index)
(import ./lookup)
(import ./parser)
(import ./position)
(import ./server-utils)
(import ./uri)
(import spork/path)

(defn symbol-context [state params]
  (def document-uri (server-utils/document-uri params))
  (def document (server-utils/document state params))
  (def content (document :content))
  (def location (server-utils/request-byte-position state params content))
  (def word (lookup/word-at location content))
  (def name (word :word))
  (def workspace (server-utils/document-workspace state document))
  (def local-declaration
    (parser/binding-at location content (get-in document [:analysis :syntax-tree])))
  (def parser-resolved
    (and (not (empty? name))
         (not local-declaration)
         (parser/binding-definition-at location content name)))
  (def parser-resolved-binding
    (and parser-resolved
         (parser/binding-at (get-in parser-resolved [:range :start]) content
                            (get-in document [:analysis :syntax-tree]))))
  (def resolved
    (and (not (empty? name))
         (if local-declaration
           {:name name :range local-declaration}
           (and parser-resolved-binding
                {:name name :range parser-resolved-binding}))))
  (var indexed (and (not (empty? name))
                    (index/resolve-definition workspace document-uri name location)))
  (def resolved-indexed?
    (and resolved indexed
         (server-utils/same-position?
           (get-in resolved [:range :start])
           (get-in indexed [:selection-range :start]))))
  (def occurrence
    (first
      (filter |(and (server-utils/same-position?
                      (get-in $ [:range :start])
                      {:line (location :line)
                       :character (first (word :range))})
                    (= name ($ :name)))
              (get-in document [:analysis :index :references] @[]))))
  (when (and indexed
             (not (or (and occurrence
                           (= (occurrence :identity) (indexed :identity)))
                      (server-utils/same-position?
                        (get-in indexed [:selection-range :start])
                        {:line (location :line)
                         :character (first (word :range))}))))
    (set indexed nil))
  (def local (and resolved (not resolved-indexed?) resolved))
  {:uri document-uri
   :document document
   :content content
   :location location
   :word word
   :name name
   :occurrence occurrence
   :workspace workspace
   :local local
   :indexed (and (not local) indexed)})

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

    (and (not (empty? name)) (context :occurrence))
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

(defn- definition-location [state definition]
  (when-let [content (server-utils/content state (definition :uri))]
    {:uri (definition :uri)
     :range (server-utils/lsp-range state content (definition :range))}))

(defn on-type-definition [state params]
  (def context (symbol-context state params))
  (if-let [identity (get-in context [:indexed :identity])
           definition (index/type-definition (context :workspace) identity)
           location (definition-location state definition)]
    [:ok state location]
    [:ok state :null]))

(defn on-implementation [state params]
  (def context (symbol-context state params))
  (if-let [identity (get-in context [:indexed :identity])]
    [:ok state
     (catseq [definition :in (index/implementations (context :workspace) identity)
              :let [location (definition-location state definition)]
              :when location]
       location)]
    [:ok state :null]))

(defn- document-symbol [state content definition definitions]
  {:name (definition :name)
   :kind (definition :kind)
   :range (server-utils/lsp-range state content (definition :range))
   :selectionRange (server-utils/lsp-range state content
                                            (definition :selection-range))
   :children
   (array
     ;(map |{:name ($ :name)
             :kind ($ :kind)
             :range (server-utils/lsp-range state content ($ :range))
             :selectionRange (server-utils/lsp-range
                               state content ($ :selection-range))}
           (definition :children))
     ;(map |(document-symbol state content $ definitions)
           (filter |(= (definition :identity) ($ :container)) definitions)))})

(defn on-document-symbols [state params]
  (if-let [document (server-utils/document state params)]
    (let [record (get-in document [:analysis :index])
          definitions (record :definitions)]
      [:ok state
       (map |(document-symbol state (document :content) $ definitions)
            (filter |(nil? ($ :container)) definitions))])
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
    (let [definition-start (get-in context [:local :range :start])
          record (get-in context [:document :analysis :index])
          indexed-definition
          (first (filter |(server-utils/same-position?
                            definition-start (get-in $ [:selection-range :start]))
                         (record :definitions)))
          identity (if indexed-definition
                     (indexed-definition :identity)
                     (string (context :uri) "#local:"
                             (definition-start :line) ":"
                             (definition-start :character)))]
      (each reference (record :references)
        (when (= identity (reference :identity))
          (array/push locations {:uri (context :uri) :range (reference :range)})))
      (when (and include-declaration
                 (not (any? (map |(server-utils/same-position?
                                    definition-start (get-in $ [:range :start]))
                                 locations))))
        (array/push locations {:uri (context :uri)
                               :range (get-in context [:local :range])})))
    (each reference
          (index/references-by-identity (context :workspace)
                                        (get-in context [:indexed :identity]))
      (array/push locations {:uri (reference :uri) :range (reference :range)})))
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
  (if (or (empty? (context :name))
          (not (or (context :local) (context :indexed))))
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

(defn on-document-highlights [state params]
  (def context (symbol-context state params))
  (if (or (empty? (context :name))
          (not (or (context :local) (context :indexed))))
    [:ok state @[]]
    (let [document-uri (context :uri)
          definition (or (context :local) (context :indexed))
          definition-uri (if (context :local)
                           document-uri
                           (definition :uri))
          definition-start
          (get-in definition [(if (context :local) :range :selection-range) :start])
          highlights
          (catseq [reference :in (raw-references context true)
                   :when (= document-uri (reference :uri))]
            {:range (server-utils/lsp-range state (context :content)
                                           (reference :range))
             :kind (if (and (= definition-uri document-uri)
                            (server-utils/same-position?
                              definition-start
                              (get-in reference [:range :start])))
                     3
                     2)})]
      [:ok state
       (sort-by |[(get-in $ [:range :start :line])
                  (get-in $ [:range :start :character])]
                (distinct highlights))])))

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
