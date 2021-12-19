(ns de.gutzufusss.durakbot.helper.crypto
  (:require [clojure.tools.logging :as logger])
  (:import (java.security MessageDigest)
           (java.math BigInteger)
           (java.util Base64)))

(defn md5
  [^String s]
  {:post [(do (logger/debugf "md5 hashed \"%s\" -> \"%s\" " s %)
              true)]}
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn base64
  [^String s encode]
  {:post [(do (logger/debugf "base64 %s \"%s\" -> \"%s\" "
                             (if encode "encoded" "decoded") s %)
              true)]}
  (if encode
    (.encodeToString (Base64/getEncoder) (.getBytes s))
    (String. (.decode (Base64/getDecoder) s))))
