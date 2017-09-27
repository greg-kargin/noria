(ns server-ws.noria
  (:require [clojure.spec.alpha :as s]))

(s/def ::element-type (s/or :primitive keyword? :user fn?))
(s/def ::props (s/map-of keyword? any?))
(s/def ::element (s/spec (s/cat :type ::element-type :props (s/? ::props) :children (s/* ::element))))
(s/def ::key any?)
(s/def ::element-with-key (s/keys :req-un [::element ::key]))



(s/def :component/element ::element-with-key)
(s/def :component/key ::key)
(s/def ::node any?)
(s/def :component/node ::node)

(s/def :component/subst ::component)

(s/def ::user-component (s/keys :req [:component/node
                                      :component/key
                                      :component/element
                                      :component/subst]))

(s/def :component/children (s/coll-of ::component))
(s/def ::primitive-component (s/keys :req [:component/node
                                           :component/key
                                           :component/element
                                           :component/children]))

(s/def ::component (s/or ::user-compomnent ::primitive-component))

(defmulti update-spec :update/type)
(s/def ::update (s/multi-spec update-spec :update/type))
(s/def :make-node/node ::node)
(s/def :make-node/props ::props)
(s/def :make-node/type keyword?)

(defmethod update-spec :make-node [_] (s/keys :req [:make-node/node
                                                    :make-node/props
                                                    :make-node/type]))

(s/def :update-props/node ::node)
(s/def :update-props/props-diff ::props)

(defmethod update-spec :update-props [_] (s/keys :req [:update-props/node
                                                       :update-props/props-diff]))

(s/def :remove/node ::node)
(defmethod update-spec :remove [_] (s/keys :req [:remove/node]))

(s/def :destroy/node ::node)
(defmethod update-spec :destroy [_] (s/keys :req [:destroy/node]))

(s/def :add/index nat-int?)
(s/def :add/parent ::node)
(s/def :add/child ::node)

(defmethod update-spec :add [_] (s/keys :req [:add/index
                                              :add/parent
                                              :add/child]))


(defn longest [xs ys] (if (> (count xs) (count ys)) xs ys))

(defn lcs [a b]
  (let [impl* (atom nil)
        impl (memoize
              (fn [[x & xs] [y & ys]]
                (cond
                  (or (= x nil) (= y nil) ) nil
                  (= x y) (cons x (@impl* xs ys))
                  :else (longest (@impl* (cons x xs) ys) (@impl* xs (cons y ys))))))]
    (reset! impl* impl)
    (impl a b)))

(defprotocol Toolkit
  (make-node [tk e])
  (perform-updates [tk u]))

(defn get-children [[_ props & children]]
  (cond
    (map? props) children
    (some? props) (cons props children)
    :else children))

(defn get-props [[_ props & children]]
  (if (map? props) props nil))

(defn get-key [[type :as e] indices]
  (if-let [key (or (:key (get-props e)) (:key (meta e)))]
    [key indices]
    (let [indices' (update indices type (fn [i] (if i (inc i) 0)))
          key (indices' type)]
      [[type key] indices'])))

(defn user-component? [[type]]
  (fn? type))

(defn elts-with-keys [children]
  (second (reduce (fn [[indices res] e]
                    (let [[key indices'] (get-key e indices)]
                      [indices' (conj res {:elt e
                                           :key key})]))
                  [{} []] children)))

(def get-type first)

(defn build-component [{[type & args :as elt] :elt key :key} tk]
  (if (user-component? elt)
    (let [subst (apply type args)
          [c-subst updates] (build-component {:elt subst
                                              :key key} tk)]
      [{:component/node (:component/node c-subst)
        :component/key key
        :component/element elt
        :component/subst c-subst}
       updates])
    (let [new-node (make-node tk elt)
          built (map #(build-component % tk) (elts-with-keys (get-children elt)))
          c-components (map first built)
          updates (mapcat second built)]
      [{:component/node new-node
        :component/key key
        :component/element elt
        :component/children c-components}
       (concat [{:update/type :make-node
                 :make-node/node new-node
                 :make-node/type (get-type elt)
                 :make-node/props (get-props elt)}]
               updates
               (map-indexed (fn [i c]
                              {:update/type :add
                               :add/index i
                               :add/parent new-node
                               :add/child (:component/node c)})
                            c-components))])))

(declare reconcile)

(defn reconcile-children [{c-children :component/children
                           c-element :component/element
                           c-key :component/key
                           c-node :component/node :as c} e tk]
  (let [new-children (elts-with-keys (get-children e))
        old-keys (map :component/key c-children)
        new-keys (map :key new-children)]
    (if (not= old-keys new-keys)
      (let [common (into #{} (lcs old-keys new-keys))
            key->component (->> c-children
                                (map (fn [{key :component/key :as c}] [key c]))
                                (into {}))

            recon (map (fn [{e :elt
                            k :key :as child}]
                         (if-let [old-c (key->component k)]
                           (reconcile old-c child tk)
                           (build-component child tk)))
                       new-children)

            new-keys-set (into #{} new-keys)

            removes (->> c-children
                         (remove #(contains? common (:component/key %)))
                         (mapcat
                          (fn [{node :component/node
                               key :component/key}]
                            (cond-> [{:update/type :remove
                                      :remove/node node}]
                              (not (contains? new-keys-set key))
                              (conj {:update/type :destroy
                                     :destroy/node node})))))

            new-c-children (map first recon)

            adds (->> new-c-children
                      (keep-indexed (fn [i {k :component/key
                                           e :component/element
                                           child-node :component/node}]
                                      (when-not (contains? common k)
                                        {:update/type :add
                                         :add/index i
                                         :add/child child-node
                                         :add/parent c-node}))))]
        [(map first recon) (concat removes (mapcat second recon) adds)])
      (let [recon (map (fn [c elt]
                         (reconcile c elt tk))
                       c-children (get-children e))]
        [(map first recon) (mapcat second recon)]))))

(defn reconcile-primitive [{old-e :component/element
                            node :component/node :as c} {elt :elt :as e} tk]
  (if (not= (get-type old-e) (get-type elt))
    (let [[new-c updates] (build-component e tk)]
      [new-c (concat updates [{:update/type :remove
                               :remove/node node}
                              {:update/type :destroy
                               :destroy/node node}])])
    (let [[children-reconciled children-updates] (reconcile-children c elt tk)
          [old-props new-props] [(get-props old-e) (get-props elt)]
          props-updates (when (not= old-props new-props)
                          [{:update/type :update-props
                            :update-props/node node
                            :update-props/new-props new-props
                            :update-props/old-props old-props}])]
      [(assoc c
         :component/element elt
         :component/children children-reconciled)
       (concat props-updates children-updates)])))

(defn reconcile-user [{c-subst :component/subst
                       old-elt :component/element :as c} {[type & args :as elt] :elt key :key} tk]
  (let [subst (if (not= elt old-elt)
                (apply type args)
                (:component/element c-subst))
        [new-c-subst updates] (reconcile c-subst {:elt subst
                                                  :key key} tk)]
      [(assoc c
              :component/subst new-c-subst
              :component/element elt
              :component/node (:component/node new-c-subst))
       updates]))

(defn reconcile [c e tk]
  (if (user-component? (:elt e))
    (reconcile-user c e tk)
    (reconcile-primitive c e tk)))

(comment

  (defn label [x]
    [:label {:text (str x)}])

  (defn labels [x]
    (into [:div]
          (map (fn [i] [label i]) (range x))))


  (defn tt []
    (let [x (atom 0)]
      (reify Toolkit
        (make-node [tk e]
          (swap! x inc))
        (perform-updates [tk u]
          (prn :perform-updates u)
          u))))

  (def tk (tt))

  (def *counter (atom 0))
  
  (defn gen-elems [n update-fn *counter]
    (map (fn [idx] [:div {:on-click (fn []
                                     (swap! *counter inc)
                                     (update-fn))}
                   [:text {:text (str idx)}]]) (range n)))

  (def *c (atom nil))

  (defn update! []
    (let [[c' u] (reconcile @*c {:elt (into
                                             [:div {:on-click (fn []
                                                                (swap! *counter inc)
                                                                (update!))}]
                                             (gen-elems @*counter update! *counter))
                                       :key 0} tk)]
      (reset! *c c')
      (perform-updates tk u)))
  
  (def cc (first (build-component {:elt (into
                                           [:div {:on-click (fn []
                                                              (swap! *counter inc)
                                                              (update!))}]
                                           (gen-elems @*counter update! *counter))
                                   :key 0} tk)))
  (reset! *c cc)
  

  (def on-click (-> @*c :component/element second :on-click))

  (on-click)
  (reset! *c comp)
  cc 
  #:component {:node 1, :key 0, :element [:div {:on-click #function[server-ws.noria/fn--31383]}], :children ()}
  @*c
  ({:update/type :update-props, :update-props/node 1, :update-props/new-props {:on-click #function[server-ws.noria/update!/fn--31377]}, :update-props/old-props {:on-click #function[server-ws.noria/fn--31383]}}
   {:update/type :make-node, :make-node/node 2, :make-node/type :div, :make-node/props {:on-click #function[server-ws.noria/gen-elems/fn--31369/fn--31370]}}
   {:update/type :make-node, :make-node/node 3, :make-node/type :text, :make-node/props {:text "0"}}
   {:update/type :add, :add/index 0, :add/parent 2, :add/child 3}
   {:update/type :add, :add/index 0, :add/child 2, :add/parent 1})

  ({:update/type :update-props,
    :update-props/node 1,
    :update-props/new-props {:on-click #function[server-ws.noria/update!/fn--31377]}
    :update-props/old-props {:on-click #function[server-ws.noria/update!/fn--31377]}}
   {:update/type :update-props,
    :update-props/node 2,
    :update-props/new-props {:on-click #function[server-ws.noria/gen-elems/fn--31369/fn--31370]}
    :update-props/old-props {:on-click #function[server-ws.noria/gen-elems/fn--31369/fn--31370]}}
   {:update/type :make-node, :make-node/node 4, :make-node/type nil, :make-node/props nil}
   {:update/type :remove, :remove/node 3}
   {:update/type :destroy, :destroy/node 3}
   
   {:update/type :make-node, :make-node/node 5, :make-node/type :div, :make-node/props {:on-click #function[server-ws.noria/gen-elems/fn--31369/fn--31370]}}
   {:update/type :make-node, :make-node/node 6, :make-node/type :text, :make-node/props {:text "1"}}
   {:update/type :add, :add/index 0, :add/parent 5, :add/child 6}
   {:update/type :add, :add/index 1, :add/child 5, :add/parent 1}
   )

  comp

  (def comp' (first (reconcile comp {:elt [label 3] :key 0} tk)))

  comp'

  (def comp'' (reconcile comp' {:elt [labels 2] :key 0} tk))

  comp''

  )
