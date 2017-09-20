(ns client-ws.core
  (:require [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [chan <! >! put! close! timeout]]
            [clojure.string :as s]
            [reagent.core :as r])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(defn message-box [new-msg-ch]
  (let [!input-value (doto (r/atom nil)
                       (->> (set! js/window.input-value)))]
    (fn []
      [:div
       [:h3 "Send a message to the server:"]
       [:input {:type "text",
                :size 50,
                :autofocus true
                :value @!input-value
                :on-change (fn [e]
                             (reset! !input-value (.-value (.-target e))))

                :on-key-press (fn [e]
                                (when (= 13 (.-charCode e))
                                  (put! new-msg-ch @!input-value)
                                  (reset! !input-value "")))}]])))

(defn message-list [!msgs]
  [:div
   [:h3 "Messages from the server:"]
   [:ul
    (if-let [msgs (seq @!msgs)]
      (for [msg msgs]
        ^{:key msg} [:li (pr-str msg)])

      [:li "None yet."])]])

(defn message-component [!msgs new-msg-ch]
  [:div
   [message-box new-msg-ch]
   [message-list !msgs]])

(defn add-msg [msgs new-msg]
  ;; we keep the most recent 10 messages
  (->> (cons new-msg msgs)
       (take 10)))

(defn receive-msgs! [!msgs server-ch]
  ;; every time we get a message from the server, add it to our list
  (go-loop []
      (let [{:keys [message error] :as msg} (<! server-ch)]
        (swap! !msgs add-msg (cond
                               error {:error error}
                               (nil? message) {:type :transit-json}
                               message message))

        (when message
          (recur)))))

(defn send-msgs! [new-msg-ch server-ch]
  ;; send all the messages to the server
  (go-loop []
      (when-let [msg (<! new-msg-ch)]
        (>! server-ch {:value msg})
        (recur))))

(set! (.-onload js/window)
      (fn []
        (go
          (let [{:keys [ws-channel error]} (<! (ws-ch "ws://localhost:8000/ws"
                                                      {:format :transit-json}))]

            (if error
              ;; connection failed, print error
              (r/render-component
               [:div
                "Couldn't connect to websocket: "
                (pr-str error)]
               js/document.body)

              (let [ ;; !msgs is a shared atom between the model (above,
                    ;; handling the WS connection) and the view
                    ;; (message-component, handling how it's rendered)
                    !msgs (doto (r/atom [])
                            (receive-msgs! ws-channel))

                    ;; new-msg-ch is the feedback loop from the view -
                    ;; any messages that the view puts on here are to
                    ;; be sent to the server
                    new-msg-ch (doto (chan)
                                 (send-msgs! ws-channel))]

                ;; show the message component
                (r/render-component
                 [message-component !msgs new-msg-ch]
                 js/document.body)))))))
