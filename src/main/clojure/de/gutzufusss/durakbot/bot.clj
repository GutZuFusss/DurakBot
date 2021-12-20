(ns de.gutzufusss.durakbot.bot
  (:require [clojure.tools.logging :as log]
            [de.gutzufusss.durakbot.helper.crypto :as crypto]
            [de.gutzufusss.durakbot.helper.util :as util]
            [clojure.string :as str]
            [manifold.deferred :as d]
            [manifold.stream :as s]
            [aleph.tcp :as tcp]
            [gloss.core :as gloss]
            [gloss.io :as io]))

(def client-info {:tz "+02:00"
                  :p 10
                  :pl "iphone"
                  :l "ru"
                  :d "iPhone8,4"
                  :ios "14.4"
                  :v "1.9.1.2"
                  :n "durak.ios"
                  :msg-type :c})

;; probably outdated, reversing the android app was no fun, so as long as this one
;; does not cause any problems (i suspect shadow bans)...
(def key-hash-salt "oc3q7ingf978mx457fgk4587fg847")

(def personal-auth-token (System/getenv "DURAK_HEHE"))
(def visible-token-chars 6)

(defn key->verified-key
  "Takes a key and performs the necessary magic. If this ever stop working, check if
  the salt is still up to date."
  [key]
  (let [salted-key (str key key-hash-salt)]
    (crypto/base64 (crypto/md5 salted-key)
                   true)))

(defn marshal
  "Serializes Clojure data and returns the result as a string."
  [data]
  (when (some? data)
    (let [msg-type (name (:msg-type data))
          data (dissoc data :msg-type)]
      (-> (str msg-type (util/clj->json data))
          (str "\n")
          (str/replace "{}" "")))))

(defn unmarshal
  "De-serializes a string of data and returns Clojure data."
  [^String data]
  (if (or (nil? data) (str/blank? data))
    nil
    (let [first-brace (or (str/index-of data "{") (count data))
          msg-type (keyword (str/trim (subs data 0 first-brace)))
          data (str/trim (subs data first-brace (count data)))]
      (assoc (util/json->clj data) :msg-type msg-type))))

(defn session-key-request
  "Initiation of the handshake."
  []
  (assoc client-info :t (util/current-iso8601)))

(defn verify-key-request
  "This is the crucial part of the handshake. We put some salt on our key, MD5 hash it
  and then proceed to base64 encode the whole mess. Returns the map containing the
  information to build the corresponding request."
  [key]
  (do (log/infof "received session key \"%s\", computing verification challenge."
                 key)
      {:hash (key->verified-key key)
       :msg-type :sign}))

(defn auth-request
  "Use your authentication token to build the auth request."
  [token]
  (do (log/infof "using authentication token \"%s\"."
                 (str (subs personal-auth-token 0 visible-token-chars)
                      (reduce str (repeat (- (count token) visible-token-chars)
                                          "*"))))
      {:token token
       :msg-type :auth}))

(defn handle-server-info
  "This might eventually be one of the functions where we will have state. For now
  we just log."
  [id time]
  (log/infof "current server (id \"%s\") reports to have a server time %s."
             id
             time))

(defn handle-user-update
  "Might also at some point have state."
  [field value]
  (do (log/debugf "user data update: %s=%s" field value)
      (case field
        "new_msg"
        (when  (= "true" value)
          (log/info "you have unread messages!"))

        "points"
        (log/infof "you currently have %s points." value)

        ;default
        nil)))

(defn compute-reply
  "Callback function that is invoked when a message by the server has been received."
  [message]
  (condp = (:msg-type message)
    nil
    nil

    :sign
    (verify-key-request (:key message))

    :confirmed
    (log/debug "server confirmed session key verification.")

    :server
    (do (handle-server-info (:id message) (:time message))
        (auth-request personal-auth-token))

    :authorized
    (log/infof "authentication successfull. server reports id \"%s\"." (:id message))

    :uu
    (do (handle-user-update (:k message) (:v message))
        nil)

    :free ;; not sure what this is -- {:count 1450, :delay 23400, :msg-type :free}
    (log/debugf "msg-type free: %s" (str message))

    :tour
    (log/debugf "msg-type tour: %s" (str message))

    ;default
    (log/warnf "unknown msg-type \"%s\"!\nfull message: %s" (:msg-type message)
               message)))

(def protocol
  (gloss/compile-frame
    (gloss/delimited-frame "\n"
                           (gloss/string :utf-8))
    marshal
    unmarshal))

(defn wrap-duplex-stream
  "Wraps output and input stream. This is needed for the tcp client of the aleph lib."
  [protocol s]
  (let [out (s/stream)
        stream-map (s/map #(io/encode protocol %) out)]
    (s/connect stream-map
               s)
    (s/splice out
              (io/decode-stream s protocol))))

(defn tcp-client
  "Creates our input and output stream."
  [host port]
  (d/chain (tcp/client {:host host, :port port})
           #(wrap-duplex-stream protocol %)))

(defn init-event-loop
  "Start by initiating the handshake, then enter an endless loop and evaluate the
  servers messages."
  [client]
  (do (s/put! client (session-key-request))
      (loop []
        (let [message @(s/take! client)
              reply (compute-reply message)]
          (if-not message
            (recur)
            (do (log/debugf "replying to message: \"%s\" -> \"%s\"" message reply)
                (when reply
                  (s/put! client reply))
                (Thread/sleep 20) ;; be nice :)
                (recur)))))))

(defn -main
  []
  (let [client @(tcp-client "65.21.92.165" 10773)]
    (init-event-loop client)))
