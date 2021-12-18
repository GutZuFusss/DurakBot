(ns bot
  (:require [api]))

(def servers (api/fetch-server-list))

(defn do-smth-lawl
  []
  (let [server (rand-nth servers)
        socket (api/connect (:name server) (:host server) (:port server))
        session-key (api/request-session-key socket)]
    (api/verify-session-key socket session-key)))

(defn -main
  []
  (do-smth-lawl))
