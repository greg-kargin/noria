(ns server-ws.server
  (:require [aleph.netty :as netty]
            [aleph.http :as http]
            [manifold.stream :as stream]
            [manifold.deferred :as d]
            [manifold.bus :as bus]
            [cognitect.transit :as transit]
            [clojure.core.async :as a]
            [server-ws.noria :as noria])
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]))

(def port 8000)

(def bufsize 1024)

(def non-websocket-request
  {:status 400
   :headers {"content-type" "application/text"}
   :body "Expected a WebSocket request."})

(defonce endpoints (atom []))

(defn encode
  ([data] (encode data nil))
  ([data {:keys [write-handlers rewrite-outgoing]}]
   (let [out (ByteArrayOutputStream. 4096)
         writer (transit/writer out :json {:handlers write-handlers})]
     (transit/write writer (cond-> data
                             (some? rewrite-outgoing) (rewrite-outgoing)))
     (.toString out))))

(defn decode
  ([data] (decode data nil))
  ([transit-data {:keys [read-handlers rewrite-incoming]}]
   (let [in (ByteArrayInputStream. (.getBytes transit-data))
         reader (transit/reader in :json {:handlers read-handlers})]
     (cond-> (transit/read reader)
       (some? rewrite-incoming) (rewrite-incoming)))))

(defn make-endpoint []
  {:incoming (a/chan bufsize)
   :outgoing (a/chan bufsize)})

(defn make-remote-tk [callbacks {:keys [incoming outgoing]}]
  (let [x (atom 0)
        register-callbacks (fn [props node]
                             (into {}
                                   (map (fn [[k v]]
                                          (if (fn? v)
                                            (do
                                              (swap! callbacks assoc [k node] v)
                                              [k :handler])
                                            [k v]))
                                        props)))
        remove-callbacks (fn [props]
                           (into {}
                                 (remove (fn [[k v]] (fn? v)) props)))]
    (reify noria/Toolkit
      (make-node [tk e] (swap! x inc))
      (perform-updates [tk u]
        (->> u
             (map (fn [{type :update/type :as u}]
                    (cond (= :make-node type)
                          (update u :make-node/props register-callbacks (:make-node/node u))
                          (= :update-props type)
                          (-> u
                              (update :update-props/new-props register-callbacks (:update-props/node u))
                              (update :update-props/old-props remove-callbacks))
                          :else u)))
             (a/put! outgoing))))))

(defn run-event-loop [{:keys [incoming outgoing] :as endpoint}]
  (let [callbacks (atom {})
        tk (make-remote-tk callbacks endpoint)
        *text (atom "hello world")
        *c (atom nil)
        update! (fn update! []
                  (prn :update)
                  (let [[c' u] (noria/reconcile @*c {:elt [:div {:text @*text
                                                                 :on-click (fn []
                                                                             (swap! *text str "!")
                                                                             (update!))}]
                                                     :key 0} tk)]
                    (reset! *c c')
                    (noria/perform-updates tk u)))
        [c u] (noria/build-component {:elt [:div {:text @*text
                                                  :on-click (fn []
                                                              (swap! *text str "!")
                                                              (update!))}]
                                      :key 0}
                                     tk)]
    (reset! *c c)
    (noria/perform-updates tk u)
    (a/go (loop []
            (when-let [cb (a/<! incoming)]
              (prn cb callbacks)
              ((get @callbacks cb))
              (recur))))))

(defn connect-handler [req]
  (let [{:keys [incoming outgoing] :as endpoint} (make-endpoint)
        socket @(http/websocket-connection req {:max-frame-payload (* 32 1024 1024)})]
    (stream/connect (stream/map decode socket)
                    (stream/->sink incoming)
                    {:downstream? true})
    (stream/connect (stream/map encode (stream/->source outgoing))
                    socket #_(stream/map encode socket)
                    {:upstream? true})
    (run-event-loop endpoint)))

(defn start-server []
  (http/start-server #'connect-handler {:port port
                                         :netty {"child.reuseAddress" true,
                                                 "reuseAddress" true,
                                                 "child.keepAlive" true,
                                                 "child.connectTimeoutMillis" 100,
                                                 "tcpNoDelay" true,
                                                 "readWriteFair" true,
                                                 "child.tcpNoDelay" true}}))

(defn -main [& args]
  (println (str "server-started: " port))
  (netty/wait-for-close
   (start-server)))

(comment

  (.setOption server "tcpNoDelay" true)

  (start-server)
  (.close server)

  (def server (http/start-server #'connect-handler {:port 8000}))

  (def client @(http/websocket-client "ws://localhost:8000"))

  (stream/put! client
          "42")

  (stream/take! client)



  #_(:cljs
     (defn encode
       ([data] (encode data nil))
       ([data {::keys [write-handlers rewrite-outgoing]}]
        (let [w (transit/writer :json {:handlers write-handlers})]
          (transit/write w (cond-> data
                             (some? rewrite-outgoing) (rewrite-outgoing)))))))

  #_(:cljs
     (defn decode
       ([transit-data] (decode transit-data nil))
       ([transit-data {::keys [read-handlers rewrite-incoming]}]
        (let [r (transit/reader :json {:handlers read-handlers})]
          (cond-> (transit/read r transit-data)
            (some? rewrite-incoming) (rewrite-incoming))))))



  )
