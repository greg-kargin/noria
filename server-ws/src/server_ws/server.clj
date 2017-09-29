(ns server-ws.server
  (:require [aleph.netty :as netty]
            [aleph.http :as http]
            [manifold.stream :as stream]
            [manifold.deferred :as d]
            [manifold.bus :as bus]
            [clojure.core.async :as a]
            [server-ws.noria :as noria]
            [clojure.data.json :as json])
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]
           [java.util Date]
           [io.netty.channel ChannelOption]))

(defn now [] (new java.util.Date))

(def port 8000)
(def bufsize 1024)

(defonce endpoints (atom []))

(defn encode [data]
  (json/write-str data :key-fn (fn [k]
                                 (if (some? (namespace k))
                                   (str (namespace k) "_" (name k))
                                   (name k)))))

(defn decode [data]
  (json/read-str data))


(defn make-endpoint []
  {:incoming (a/chan bufsize)
   :outgoing (a/chan bufsize)})

(defn props-diff! [callbacks node old-props new-props]
  (let [old-props' (into {}
                         (map
                          (fn [[k v]]
                            (if (fn? v)
                              (do
                                (swap! callbacks dissoc [k node])
                                [k :noria-handler])
                              [k v]))
                          old-props))
        new-props' (into {}
                         (map
                          (fn [[k v]]
                            (if (fn? v)
                              (do
                                (swap! callbacks assoc [k node] v)
                                [k :noria-handler])
                              [k v]))
                          new-props))
        all-keys (into #{} (concat (keys old-props') (keys new-props')))]
    (into {} (keep (fn [k]
                     (let [new-val (get new-props' k)
                           old-val (get old-props' k)]
                       (when (not= new-val old-val)
                         (if (= old-val :noria-handler)
                           [k :-noria-handler]
                           [k new-val])))) all-keys))))

(defn perform-updates! [callbacks outgoing u]
  (->> u
       (map (fn [{type :update/type :as u}]
              (cond (= :make-node type)
                    (let [{:make-node/keys [node props]} u]
                      (assoc u :make-node/props (props-diff! callbacks node nil props)))
                    (= :update-props type)
                    (let [{:update-props/keys [node old-props new-props]} u]
                      (-> u
                          (assoc :update-props/props-diff (props-diff! callbacks node old-props new-props))
                          (dissoc :update-props/old-props :update-props/new-props)))
                    :else u)))
       (remove (fn [u]
                 (and (= (:update/type u) :update-props)
                      (empty? (:update-props/props-diff u)))))
       (a/put! outgoing)))

(def my-comp (noria/component (fn [n click]
                                (if (< 0 n)
                                  [:div
                                   [:text {:text (str n)}]
                                   [my-comp (dec n) click]
                                   [my-comp (dec n) click]]
                                  [:div {:on-click click}
                                   [:text {:text (str n)}]]))))

(defn gen-elems [n update-fn *counter]
  [my-comp n (fn []
               (swap! *counter inc)
               (update-fn))])

(defn run-event-loop [{:keys [incoming outgoing] :as endpoint}]
  (let [callbacks (atom {})
        *counter (atom 3)
        *c (atom nil)
        *next-id (atom 0)
        update! (fn update! []
                  (let [[c' ctx] (noria/reconcile @*c {:elt (into
                                                             [:div
                                                              (gen-elems @*counter update! *counter)])
                                                       :key 0} {:next-id @*next-id
                                                       :updates (transient [])})]
                    (reset! *c c')
                    (reset! *next-id (:next-id ctx))
                    (perform-updates! callbacks outgoing (persistent! (:updates ctx)))))
        [c ctx] (noria/build-component
                                {:elt (into
                                       [:div
                                        (gen-elems @*counter update! *counter)])
                                 :key 0} {:next-id @*next-id
                                          :updates (transient [])})]
    (reset! *c c)
    (reset! *next-id (:next-id ctx))
    (perform-updates! callbacks outgoing (persistent! (:updates ctx)))
    (a/go (loop []
            (when-let [{:strs [node key arguments] :as msg} (a/<! incoming)]
              (prn msg)
              (when-let [cb (get @callbacks [(keyword key) node])]                
                (apply cb arguments))
              (recur))))))

(defn connect-handler [req]
  (let [{:keys [incoming outgoing] :as endpoint} (make-endpoint)
        socket @(http/websocket-connection req {:max-frame-payload (* 32 1024 1024)})]
    (stream/connect (stream/map decode socket)
                    (stream/->sink incoming)
                    {:downstream? true})
    (stream/connect (stream/map encode (stream/->source outgoing))
                    socket
                    {:upstream? true})
    (run-event-loop endpoint)))

(defn start-server []
  (http/start-server #'connect-handler {:port 8000
                                        :bootstrap-transform #(.childOption % ChannelOption/TCP_NODELAY true)}))

(defn -main [& args]
  (println (str "server-started: " port))
  (netty/wait-for-close
   (start-server)))


(comment 

  (json/write-str {:update/type :make-node
                   } )
  (json/json-str )

  (str :a/b)
  (.setOption server "tcpNoDelay" true)

  (def server (start-server))
  (.close server)

  (def client @(http/websocket-client "ws://localhost:8000"))

  (stream/put! client
          "42")

  (stream/take! client)

)
