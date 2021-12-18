(ns api
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clj-sockets.core :as sock]
            [clojure.tools.logging :as logger]
            [clojure.string :as str])
  (:import (java.time LocalDateTime)))

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

(defn current-iso8601
  "Yeah."
  []
  (str (subs (.toString (LocalDateTime/now)) 0 23) "Z"))

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
