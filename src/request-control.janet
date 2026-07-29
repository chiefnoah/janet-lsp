(import ./server-utils)

(defn cancelled? [state request-id]
  (and request-id (has-key? (state :cancelled-requests) request-id)))

(defn checkpoint [state request-id]
  (ev/sleep 0.001)
  (when (cancelled? state request-id) (error :request-cancelled)))

(defn content [state cache uri]
  (if (has-key? cache uri)
    (let [cached (get cache uri)]
      (if (= :request-control/missing cached) nil cached))
    (let [source (server-utils/content state uri)]
      (put cache uri (or source :request-control/missing))
      source)))
