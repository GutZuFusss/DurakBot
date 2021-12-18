(ns bot
  (:require [api]))

(def servers (api/fetch-server-list))

(defn do-smth-lawl
  []
  (let [server (rand-nth servers)]
    (api/connect (:name server) (:host server) (:port server))))

(defn -main
  []
  (do #_(.setLevel (Logger/getLogger (str *ns*)) Level/ALL)
      (do-smth-lawl)))
