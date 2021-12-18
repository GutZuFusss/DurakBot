(ns api
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clj-sockets.core :as sock]
            [clojure.tools.logging :as logger]))

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

(defn current-iso8601
  "Yeah."
  []
  (str (subs (.toString (java.time.LocalDateTime/now)) 0 23) "Z"))

(defn json->clj
  "Takes a json string, converts it to Clojure data and keywordizes the keys of any
  maps from the data."
  [json]
  (json/read-str json
                 :key-fn keyword))

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

(defn get-session-key
  [socket]
  (let [client-info (assoc client-info :t current-iso8601)]))

