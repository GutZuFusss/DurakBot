(ns de.gutzufusss.durakbot.helper.util
  (:require [clojure.data.json :as json]
            [clojure.string :as str])
  (:import (java.time LocalDateTime)))

(defn current-iso8601
  "Yeah. Don't look at me."
  []
  (str (subs (.toString (LocalDateTime/now)) 0 23) "Z"))

(defn json->clj
  "Takes a json string, converts it to Clojure data and keywordizes the keys of any
  maps from the data."
  [json-string]
  (if (str/blank? json-string)
    {}
    (json/read-str json-string
                   :key-fn keyword)))

(defn clj->json
  "Takes Clojure data and converts it to a json string."
  [data]
  (json/write-str data))
