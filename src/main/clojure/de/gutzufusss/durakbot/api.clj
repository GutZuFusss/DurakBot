(ns de.gutzufusss.durakbot.api
  (:require [clj-http.client :as client]
            [clj-sockets.core :as sock]
            [clojure.tools.logging :as logger]
            [clojure.string :as str]
            [de.gutzufusss.durakbot.helper.crypto :as crypto]
            [de.gutzufusss.durakbot.helper.util :as util]))

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
(def accepted-token-response "confirmed") ;; this one is just plain text, no json
(def server-greeting-command "server")
(def init-registration-command "get_captcha")
(def finalize-registration-command "register")
(def set-token-command "set_token")
(def authenticate-command "auth")
(def error-command "err")
(def create-game-command "create")

(def key-hash-salt "oc3q7ingf978mx457fgk4587fg847") ;; obtained by reversing the ios app

(defn key->weird-hash-thing
  "Takes a key and performs the necessary magic. If this ever stop working, check if
  the salt is still the same."
  [key]
  (let [salted-key (str key key-hash-salt)]
    (crypto/base64 (crypto/md5 salted-key)
                   true)))

(defn marshal
  "Serializes data (everything is loosely based on some whack reverse engineering)."
  [data]
  (if (nil? data)
      nil
      (let [command (:command data)
            data (dissoc data :command)]
        ;(.getBytes "some string" "UTF-8") <--- TODO: test thoroughly if we need that
        (str/replace (str command (util/clj->json data) "\n")
                     "{}"
                     ""))))

(defn unmarshal
  "De-serializes data."
  [data]
  (if (or (nil? data) (str/blank? data))
    nil
    (let [first-brace (str/index-of data "{")
          command (str/trim (subs data 0 first-brace))
          data (str/trim (subs data first-brace (count data)))]
      (assoc (util/json->clj data) :command command))))

(defn fetch-server-list
  "Fetches server list using HTTP and returns it as vector."
  []
  {:post [(do (logger/info (str "fetched the following servers: " %))
              (< 0 (count %)))]}
  (let [headers {user-agent-key user-agent-value}
        clean-map-fn #(for [server (vals %)] (select-keys server relevant-server-keys))
        eng-name-fn #(map (fn [srv-map] (assoc srv-map :name (:en (:name srv-map)))) %)]
    (-> (:user (util/json->clj
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
  server to reply with a session key, which then is returned in a map and can
  be retrieved with the :key keyword."
  [socket]
  {:post [(do (logger/info "received session token:" (:key %))
              (and (= (:command %) session-token-command)
                   (not (str/blank? (:key %)))))]}
  (let [client-info (assoc client-info :t (util/current-iso8601))]
    (do (sock/write-to socket (marshal client-info))
        (unmarshal (sock/read-line socket)))))

(defn verify-session-key
  "This is the crucial part of the handshake. We put some salt on our key, MD5 hash it
  and then proceed to base64 encode the whole mess. Returns a boolean value w.r.t.
  the outcome of the verification process."
  [socket key]
  {:post [(do (logger/infof "successfully verified session. server id is \"%s\", server time is %s."
                           (:id %) (:time %))
              (= server-greeting-command (:command %)))]}
  (let [hash (key->weird-hash-thing key)
        payload-map {:hash hash
                     :command session-token-command}]
    (do (sock/write-to socket (marshal payload-map))
        (and (= accepted-token-response (sock/read-line socket))
             (unmarshal (sock/read-line socket))))))

(defn prompt-captcha-answer
  "NOT_IMPLEMENTED"
  [captcha-url]
  (logger/warn "NOT_IMPLEMENTED"))

(defn finalize-registration
  "Send the captcha result (or nil if we did not get a captcha) along with the username.
  Will return the token needed for logging in."
  [socket username captcha]
  {:post [(do (logger/infof "account created successfully. token is \"%s\"." %)
              %)]}
  (let [register-map {:name username
                      :captcha captcha
                      :command finalize-registration-command}]
    (do (sock/write-to socket (marshal register-map))
        (logger/debug (sock/read-line socket))
        (when captcha
          (let [data (sock/read-line socket)
                _ (logger/debug data)
                data (unmarshal data)]
            (when (= set-token-command (:command data))
              (:token data)))))))


(defn register-account
  "Quality of life function to create account from Clojure. Captcha has to be entered
  manually. Will return false if something went wrong (Incorrect captcha in most cases)."
  [socket username]
  (let [request-captcha-map {:command init-registration-command}]
    (do (sock/write-to socket (marshal request-captcha-map))
        (let [response1 (unmarshal (sock/read-line socket))
              response2 (unmarshal (sock/read-line socket))
              captcha-url (or (:url response1) (:url response2))]
          (do
            (logger/debug init-registration-command "response1" response1)
            (logger/debug init-registration-command "response2" response2)
            (if (nil? captcha-url)
              (do (logger/info "no captcha url received, proceeding with registration...")
                  (finalize-registration socket username nil))
              (finalize-registration socket username (prompt-captcha-answer captcha-url))))))))

(defn authenticate
  "Use your authentication token to log in. Returns the response."
  [socket token]
  {:post [(do (logger/infof "authentication successfull. token is \"%s\"." %)
              (not= error-command (:command %)))]}
  (let [auth-map {:token token
                  :command authenticate-command}]
    (do (sock/write-to socket (marshal auth-map))
        (last
          (repeat 3
                  (let [response (unmarshal (sock/read-line socket))]
                    (if (nil? response))
                    (do (logger/debug "response to authentication:" response)
                        response)))))))

(defn create-game
  "Creates a lobby others can join. Does not return shi"
  [socket & {:keys [sw bet deck password
                    players fast ch nb]
             :or {sw false, bet 500, deck 36, password nil,
                  players 4, fast false, ch false, nb false}}]
  (let [create-game-map {:sw sw
                         :bet bet
                         :deck deck
                         :password password
                         :players players
                         :fast fast
                         :ch ch
                         :nb nb
                         :command create-game-command}]
    (do (sock/write-to socket (marshal create-game-map))
        (repeat 2
                (logger/debug "response to game creation:"
                              (unmarshal (sock/read-line socket))))
        (logger/info "game was hopefully created idk the api design seems kinda odd"))))
