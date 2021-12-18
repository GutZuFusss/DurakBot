(ns api
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clj-sockets.core :as sock]
            [clojure.tools.logging :as logger]
            [clojure.string :as str])
  (:import (java.time LocalDateTime)
           (java.security MessageDigest)
           (java.math BigInteger)
           (java.util Base64)))

(def server-list-url "http://static.rstgames.com/durak/servers.json")

(def user-agent-key "User-Agent")
(def user-agent-value "Fool/1.9.1.2 CFNetwork/1220.1 Darwin/20.3.0")

(def client-info {:tz "+02:00"
                  :p 10
                  :pl "iphone"
                  :l "ru"
                  :d "iPhone8,4"
                  :ios "14.4"
                  :v "1.9.1.2"
                  :n "durak.ios"
                  :command "c"})

(def diamond-server :u0)
(def relevant-server-keys [:name :host :port])

(def session-token-command "sign")

(def key-hash-salt "oc3q7ingf978mx457fgk4587fg847") ;; obtained by reversing the ios app

;; TODO: move utility functions to separate namespace

(defn current-iso8601
  "Yeah."
  []
  (str (subs (.toString (LocalDateTime/now)) 0 23) "Z"))

(defn md5
  [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn base64
  [^String s encode]
  (if encode
    (.encodeToString (Base64/getEncoder) (.getBytes s))
    (String. (.decode (Base64/getDecoder) s))))

(defn key->weird-hash-thing
  "Takes a key and performs the necessary magic. If this ever stop working, check if
  the salt is still the same."
  [key]
  (let [salted-key (str key key-hash-salt)]
    (base64 (md5 salted-key)
            true)))

(defn json->clj
  "Takes a json string, converts it to Clojure data and keywordizes the keys of any
  maps from the data."
  [json-string]
  (json/read-str json-string
                 :key-fn keyword))

(defn clj->json
  "Takes Clojure data and converts it to a json string."
  [data]
  (json/write-str data))

(defn marshal
  "Serializes data (everything is loosely based on some whack reverse engineering)."
  [data]
  (let [command (:command data)
        data (dissoc data :command)]
    ;(.getBytes "some string" "UTF-8") <--- TODO: test thoroughly if we need that
    (str/replace (str command (clj->json data) "\n")
                 "{}"
                 "")))

(defn unmarshal
  "De-serializes data."
  [data]
  (let [first-brace (str/index-of data "{")
        command (str/trim (subs data 0 first-brace))
        data (str/trim (subs data first-brace (count data)))]
    (assoc (json->clj data) :command command)))


(defn fetch-server-list
  "Fetches server list using HTTP and returns it as vector."
  []
  {:post [(do (logger/info (str "fetched the following servers: " %))
              (< 0 (count %)))]}
  (let [headers {user-agent-key user-agent-value}
        clean-map-fn #(for [server (vals %)] (select-keys server relevant-server-keys))
        eng-name-fn #(map (fn [srv-map] (assoc srv-map :name (:en (:name srv-map)))) %)]
    (-> (:user (json->clj
                 (:body (client/get server-list-url
                                    {:headers headers}))))
        (dissoc diamond-server)
        (clean-map-fn)
        (eng-name-fn)
        (vec))))

(defn connect
  "Connects to a given server and returns the created socket."
  [name host port]
  {:post [(do (logger/infof "connected to %s:%d (%s) successfully." host port name)
              (some? %))]}
  (sock/create-socket host port))

(defn request-session-key
  "Sends some mocked client data via the socket provided and waits for the
  server to reply with a session key, which then is returned."
  [socket]
  {:post [(do (logger/info "received session token:" (:key %))
              (and (= (:command %) session-token-command)
                   (not (str/blank? (:key %)))))]}
  (let [client-info (assoc client-info :t (current-iso8601))]
    (do (sock/write-to socket (marshal client-info))
        (unmarshal (sock/read-line socket)))))

(defn verify-session-key
  "This is the crucial part of the handshake. We put some salt on our key, MD5 hash it
  and then proceed to base64 encode the whole mess. Returns a boolean value w.r.t.
  the outcome of the verification process."
  [socket key]
  {:post [(do (logger/info "successfully verified the session key.")
              %)]}
  (let [hash (key->weird-hash-thing key)
        payload-map {:hash hash
                     :command session-token-command}]
    (do (sock/write-to socket (marshal payload-map))
        (= "confirmed" (sock/read-line socket)))))
