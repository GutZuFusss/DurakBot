(ns de.gutzufusss.durakbot.helper.crypto
  (:import (java.security MessageDigest)
           (java.math BigInteger)
           (java.util Base64)))

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