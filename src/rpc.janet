(import spork/json)
(import ./logging)

(defn encode-message [message]
  (logging/info (string/format "sending: %m" message) [:rpc])
  (json/encode message))

(defn success-response [id result]
  (encode-message {:jsonrpc "2.0" :id id :result result}))

(defn error-response [id code message &opt data]
  (def error-value @{:code code :message message})
  (when data (put error-value :data data))
  (encode-message {:jsonrpc "2.0" :id id :error error-value}))

(defn notification [message]
  (encode-message (merge {:jsonrpc "2.0"} message)))

(defn request [id method params]
  (encode-message {:jsonrpc "2.0" :id id :method method :params params}))

(defn valid-id? [id]
  (or (= id :null) (string? id) (number? id)))

(defn message-id [message]
  (if (and (dictionary? message)
           (has-key? message "id")
           (valid-id? (get message "id")))
    (get message "id")
    :null))

(defn notification? [message]
  (and (dictionary? message) (not (has-key? message "id"))))

(defn response? [message]
  (and (dictionary? message)
       (= "2.0" (get message "jsonrpc"))
       (has-key? message "id")
       (valid-id? (get message "id"))
       (not (has-key? message "method"))
       (or (has-key? message "result") (has-key? message "error"))))

(defn validate-message [message]
  (cond
    (not (dictionary? message)) [-32600 "Invalid Request" "request must be an object"]
    (not= "2.0" (get message "jsonrpc")) [-32600 "Invalid Request" "jsonrpc must be \"2.0\""]
    (not (string? (get message "method"))) [-32600 "Invalid Request" "method must be a string"]
    (and (has-key? message "id")
         (not (valid-id? (get message "id")))) [-32600 "Invalid Request" "id must be a string, number, or null"]
    (and (has-key? message "params")
         (not (or (dictionary? (get message "params"))
                  (indexed? (get message "params"))))) [-32602 "Invalid params" "params must be an object or array"]
    nil))
