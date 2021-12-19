(ns de.gutzufusss.durakbot.bot
  (:require [de.gutzufusss.durakbot.api :as api]))

(def servers (api/fetch-server-list))

(defn do-smth-lawl
  []
  (do
    (let [server (rand-nth servers)
          socket (api/connect (:name server) (:host server) (:port server))
          session-key-response (api/request-session-key socket)
          session-key (:key session-key-response)]
      (do (api/verify-session-key socket session-key)
          #_(api/register-account socket "Olagf")))))

(defn -main
  []
  (do-smth-lawl))
