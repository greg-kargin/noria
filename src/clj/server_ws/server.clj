(ns server-ws.server
  (:require [aleph.netty :as netty]
            [aleph.http :as http]
            [manifold.stream :as stream]
            [manifold.deferred :as d]
            [manifold.bus :as bus]
            [clojure.core.async :as a]
            [server-ws.noria :as noria]
            [clojure.data.json :as json]
            [andel.core :as andel]
            [andel.controller :as controller]
            [andel.intervals :as intervals]
            [andel.noria])
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

(defonce styles (atom {}))

(defn style->class [style]
  (let [name (str "style__" (hash style))]
    (when-not (contains? @styles name)
      (swap! styles assoc (str "." name) style))
    name))

(defonce next-marker-id (atom 0))

(defn- create-marker [{:keys [from to greedy-left? greedy-right? style]}]
  (letfn [(classes-by-keys [ks styles]
                           (let [classes (->> styles
                                              (map (fn [style]
                                                     (let [style (select-keys style ks)]
                                                       (when (not-empty style)
                                                         (style->class style)))) )
                                              (filter some?))]
                             (when (not-empty classes)
                               (->> classes
                                    (interpose " ")
                                    (apply str)))))]
    (intervals/->Marker from to greedy-left? greedy-right? (intervals/->Attrs (swap! next-marker-id inc)
                                                                              (classes-by-keys
                                                                                [:background-color
                                                                                 :border-bottom-style
                                                                                 :border-color
                                                                                 :border-width
                                                                                 :border-radius]
                                                                                style)
                                                                              (classes-by-keys
                                                                                [:color
                                                                                 :font-weight
                                                                                 :font-style]
                                                                                style)
                                                                              nil))))

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


(def editor-impl (slurp (clojure.java.io/file "andel/resources/public/EditorImpl.java")))
(def markup
  (->> (read-string (slurp (clojure.java.io/file "andel/resources/public/markup.txt")))
       (mapv create-marker)
       (sort-by (fn [m] (.-from m)))))

(defn run-event-loop [{:keys [incoming outgoing] :as endpoint}]
  (let [*editor (atom (-> (andel/make-editor-state)
                          (assoc-in  [:viewport :metrics] {:width 9.6
                                                           :height 16
                                                           :spacing 3})
                          (assoc-in [:viewport :view-size] [800 600])
                          (andel/insert-at-offset 0 editor-impl)
                          (andel/insert-markers markup)
                          (dissoc :log)))
        elt (fn [editor update!]
              {:elt [andel.noria/editor-component
                     editor
                     {:on-scroll (fn [dx dy]
                                   (swap! *editor controller/scroll dx dy)
                                   (update!))
                      :on-mouse-down (fn [x y]
                                       (swap! *editor (fn [state]
                                                        (let [viewport (:viewport state)
                                                              line-col (andel.utils/pixels->grid-position [x y] viewport)]
                                                          (controller/set-caret-at-grid-pos state line-col false))))
                                       (update!))}
                     @styles]
               :key 0})
        callbacks (atom {})
        *c (atom nil)
        *next-id (atom 0)
        update! (fn update! []
                  (let [[c' ctx] (noria/reconcile @*c (elt @*editor update!)
                                                  {:next-id @*next-id
                                                   :updates (transient [])})]
                    (reset! *c c')
                    (reset! *next-id (:next-id ctx))
                    (perform-updates! callbacks outgoing (persistent! (:updates ctx)))))
        [c ctx] (noria/build-component (elt @*editor update!)
                                       {:next-id @*next-id
                                        :updates (transient [])})]
    (reset! *c c)
    (reset! *next-id (:next-id ctx))
    (perform-updates! callbacks outgoing (persistent! (:updates ctx)))
    (a/go (loop []
            (when-let [{:strs [node key arguments] :as msg} (a/<! incoming)]
              (prn msg)
              (def cbcb @callbacks)
              (when-let [cb (get @callbacks [(keyword key) node])]                
                (try  (apply cb arguments)
                      (catch Exception e
                        (prn e))))
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

  (def ed (-> (andel/make-editor-state)
              (assoc-in  [:viewport :metrics] {:width 9.6
                                               :height 16
                                               :spacing 3})
              (assoc-in [:viewport :view-size] [800 600])
              (andel/insert-at-offset 0 editor-impl)
              (dissoc :log)))


  

  (noria/build-component
   {:elt [andel.noria/editor-component
          ed
          {:on-scroll (fn [dx dy]
                        )}]
    :key 0}
   
   {:next-id 0
    :updates (transient [])})
  

  (def server (start-server))
  (.close server)


)
