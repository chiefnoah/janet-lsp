(import ./index)
(import ./navigation)
(import ./request-control)
(import ./server-utils)
(import ./uri)
(import spork/path)

(defn- workspace-for-item [state item]
  (when-let [document-uri (get item "uri")
             filepath (uri/file-uri->path document-uri)]
    (server-utils/workspace-for-path state filepath)))

(defn- callable-item [state callable &opt sources]
  (default sources @{})
  (when-let [content (request-control/content state sources (callable :uri))]
    {:name (callable :name)
     :kind (callable :kind)
     :detail (string (callable :form) " in "
                     (or (and (uri/file-uri->path (callable :uri))
                              (path/basename (uri/file-uri->path (callable :uri))))
                         (callable :uri)))
     :uri (callable :uri)
     :range (server-utils/lsp-range state content (callable :range))
     :selectionRange
     (server-utils/lsp-range state content (callable :selection-range))
     :data {:identity (callable :identity)}}))

(defn- item-context [state params]
  (def item (get params "item"))
  (def identity (get-in item ["data" "identity"]))
  (def workspace (and (string? identity) (workspace-for-item state item)))
  (def callable (and workspace (index/callable-by-identity workspace identity)))
  (when (and callable (= (callable :uri) (get item "uri")))
    {:workspace workspace :callable callable :identity identity}))

(defn- cancelled? [state request-id]
  (ev/sleep 0.001)
  (and request-id (has-key? (state :cancelled-requests) request-id)))

(defn on-prepare [state params]
  (def context (navigation/symbol-context state params))
  (def occurrence-start (get-in context [:occurrence :range :start]))
  (def call
    (and occurrence-start
         (first
           (filter |(server-utils/same-position?
                       occurrence-start (get-in $ [:range :start]))
                   (get-in context [:document :analysis :index :calls] @[])))))
  (def identity
    (if call
      (call :identity)
      (or (get-in context [:occurrence :identity])
          (if-let [local (context :local)]
            (string (context :uri) "#local:"
                    (get-in local [:range :start :line]) ":"
                    (get-in local [:range :start :character]))
            (get-in context [:indexed :identity])))))
  (if-let [callable (and identity
                         (index/callable-by-identity (context :workspace) identity))
           item (callable-item state callable)]
    [:ok state [item]]
    [:ok state :null]))

(defn- grouped [calls key]
  (def groups @{})
  (each call calls
    (when-let [identity (call key)]
      (unless (get groups identity) (put groups identity @[]))
      (array/push (get groups identity) call)))
  groups)

(defn- sorted-results [results]
  (sort-by |[(get-in $ [:from :name] (get-in $ [:to :name]))
             (get-in $ [:from :uri] (get-in $ [:to :uri]))
             (get-in $ [:fromRanges 0 :start :line]
                     (get-in $ [:toRanges 0 :start :line] 0))
             (get-in $ [:fromRanges 0 :start :character]
                     (get-in $ [:toRanges 0 :start :character] 0))]
           results))

(defn on-incoming [state params request-id]
  (if-let [context (item-context state params)]
    (let [workspace (context :workspace)
           sources @{}
          groups (grouped (index/incoming-calls workspace (context :identity)) :caller)
          results @[]]
      (eachp [caller-identity calls] groups
        (when (cancelled? state request-id)
          (break))
        (when-let [caller (index/callable-by-identity workspace caller-identity)
                    item (callable-item state caller sources)
                    content (request-control/content state sources (caller :uri))]
          (array/push results
                      {:from item
                       :fromRanges
                       (map |(server-utils/lsp-range state content ($ :range))
                            (sort-by |[(get-in $ [:range :start :line])
                                      (get-in $ [:range :start :character])]
                                     calls))})))
      (if (cancelled? state request-id)
        [:rpc-error state -32800 "Request cancelled"]
        [:ok state (sorted-results results)]))
    [:ok state :null]))

(defn on-outgoing [state params request-id]
  (if-let [context (item-context state params)]
    (if-let [caller-content
              (server-utils/content state (get-in context [:callable :uri]))]
      (let [workspace (context :workspace)
           sources (do (def found @{})
                       (put found (get-in context [:callable :uri]) caller-content)
                       found)
          groups (grouped (index/outgoing-calls workspace (context :identity)) :identity)
          results @[]]
      (eachp [target-identity calls] groups
        (when (cancelled? state request-id)
          (break))
        (when-let [target (index/callable-by-identity workspace target-identity)
                    item (callable-item state target sources)]
          (array/push results
                      {:to item
                       :toRanges
                       (map |(server-utils/lsp-range state caller-content ($ :range))
                            (sort-by |[(get-in $ [:range :start :line])
                                      (get-in $ [:range :start :character])]
                                     calls))})))
      (if (cancelled? state request-id)
        [:rpc-error state -32800 "Request cancelled"]
        [:ok state (sorted-results results)]))
      [:ok state :null])
    [:ok state :null]))
