(ns client-ws.core
  (:require [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [chan <! >! put! close! timeout]]
            [clojure.string :as s]
            #_[reagent.core :as r])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

#_(defonce state (r/atom nil))

#_(defn recieve-msg [server-ch]
  (go-loop []
    (let [{:keys [message error] :as msg} (<! server-ch)]
      (reset! state message)
      (when message
        (recur)))))

#_(defn recieve-msg [ws-chan]
  (go
    (:message (<! ws-chan))))

(defmulti perform-update! :update/type)

(defonce nodes (atom {}))
(defonce callbacks (atom {:on-click #{}}))


(defmethod perform-update! :make-node
  [{:make-node/keys [node type props]}]
  (let [{:keys [text]} props
        elt (js/document.createElement type)]
    (.setAttribute elt "noria-id" node)
    (set! (.-innerHTML elt) text)
    (swap! nodes assoc node elt)
    (when (:on-click props)
      (swap! callbacks update :on-click conj node))
    (.appendChild (js/document.getElementById "root") elt)))

(defmethod perform-update! :update-props
  [{:update-props/keys [new-props node]}]
  (when-let [elt (get @nodes node)]
    (when (:on-click new-props)
      (swap! callbacks update :on-click conj node))
    (set! (.-innerHTML elt) (:text new-props))))

(defn perform-updates! [updates]
  #_(prn updates)
  (doseq [update updates]
    (perform-update! update)))

(defn send-msg [server-ch msg]
  (go
    (>! server-ch msg)))

(defn find-source [elt handlers]
  (when (some? elt)
    (let [id (.getAttribute elt "noria-id")]
      (if (contains? handlers (int id))
        (int id)
        (recur (.getParent elt) handlers)))))

(defn -main []
  (go
    (js/performance.mark "fuck-start")
    (when-let [ws-chan (:ws-channel (<! (ws-ch "ws://localhost:8000/ws" {:format :transit-json})))]
      (.addEventListener (js/document.getElementById "root")
                         "click" (fn [e]
                                   (js/performance.mark "fuck-start")
                                   (let [target (.-target e)
                                         handlers (get @callbacks :on-click)]
                                     (when (seq handlers)
                                       (when-let [elt-id (find-source target handlers)]
                                         (put! ws-chan [:on-click elt-id]))))))
      (go-loop []
        (when-let [m (:message (<! ws-chan))]
          (perform-updates! m)
          (js/performance.mark "fuck-end")
          (js/performance.measure "fuck" "fuck-start" "fuck-end"))
        (recur)))))

(defonce init
  (-main))
