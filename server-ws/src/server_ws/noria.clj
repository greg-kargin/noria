(ns server-ws.noria
  (:require [clojure.spec.alpha :as s]))

(s/def ::element-type (s/or :primitive keyword? :user fn?))
(s/def ::props (s/map-of keyword? any?))
(s/def ::element (s/spec (s/cat :type ::element-type :props (s/? ::props) :children (s/* ::element))))
(s/def ::key any?)
(s/def ::element-with-key (s/keys :req-un [::element ::key]))

(s/def :component/element ::element-with-key)
(s/def ::node any?)
(s/def :component/node ::node)

(s/def :component/subst ::component)

(s/def ::user-component (s/keys :req [:component/node
                                      :component/element
                                      :component/subst]))

(s/def :component/children (s/coll-of ::component))
(s/def ::primitive-component (s/keys :req [:component/node
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

(defn get-children [{[_ props & r] :elt}]
  (let [children (cond
                   (map? props) r
                   (some? props) (cons props r)
                   :else r)]
    (persistent! (second (reduce (fn [[indices res] e]
                                   (if-let [key (or (:key (get-props e)) (:key (meta e)))]
                                     [indices (conj res {:elt e :key key})]
                                     (let [type (first e)
                                           indices' (update indices type (fn [i] (if i (inc i) 0)))
                                           idx (indices' type)]
                                       [indices' (conj! res {:elt e :key [type idx]})])))
                                 [{} (transient [])] children)))))

(defn get-props [{[_ props & children] :elt}]
  (if (map? props) props nil))

(defn get-type [{[type] :elt}] type)

(defn user-component? [elt]
   (fn? (get-type elt)))

(defn map-children [f next-id children]
  (let [[recons* next-id'] (reduce (fn [[recons next-id] c]
                                     (let [[r next-id'] (f c next-id)]
                                       [(conj! recons r) next-id']))
                                   [(transient []) next-id]
                                   children)]
    [(persistent! recons*) next-id']))

(defn build-component [{[type & args] :elt key :key :as elt} next-id]
  (if (user-component? elt)
    (let [subst (apply type args)
          [{c-subst :component
            updates :updates} next-id'] (build-component {:elt subst
                                                          :key key} next-id)]
      [{:component {:component/node (:component/node c-subst)
                    :component/element elt
                    :component/subst c-subst}
        :updates updates} next-id'])
    (let [[built next-id'] (map-children build-component (inc next-id) (get-children elt))
          c-components (map :component built)]
      [{:component {:component/node next-id
                     :component/element elt
                     :component/children c-components}
         :updates (concat [{:update/type :make-node
                            :make-node/node next-id
                            :make-node/type type
                            :make-node/props (get-props elt)}]
                          (mapcat :updates built)
                          (map-indexed (fn [i c]
                                         {:update/type :add
                                          :add/index i
                                          :add/parent next-id
                                          :add/child (:component/node c)})
                                       c-components))} next-id'])))

(declare reconcile)

(defn reconcile-children [{c-children :component/children
                           c-node :component/node :as c} new-children next-id]
  (let [old-keys (map (comp :key :component/element) c-children)
        new-keys (map :key new-children)]
    (if (not= old-keys new-keys)
      (let [common (into #{} (lcs old-keys new-keys))
            key->component (->> c-children
                                (map (fn [c] [(:key (:component/element c)) c]))
                                (into {}))

            [recon next-id'] (map-children (fn [child next-id]
                                             (if-let [old-c (key->component (:key child))]
                                               (reconcile old-c child next-id)
                                               (build-component child next-id)))
                                           next-id new-children)

            new-keys-set (into #{} new-keys)

            removes (->> c-children
                         (remove #(contains? common (-> % :component/element :key)))
                         (mapcat
                          (fn [{node :component/node
                               elt :component/element}]
                            (let [remove {:update/type :remove
                                          :remove/node node}]
                              (if (contains? new-keys-set (:key elt))
                                [remove]
                                [remove {:update/type :destroy
                                         :destroy/node node}])))))

            new-c-children (map :component recon)

            adds (->> new-c-children
                      (keep-indexed (fn [i {elt :component/element
                                           child-node :component/node}]
                                      (when-not (contains? common (:key elt))
                                        {:update/type :add
                                         :add/index i
                                         :add/child child-node
                                         :add/parent c-node}))))]
        [{:components (map :component recon)
          :updates (concat removes (mapcat :updates recon) adds)}
         next-id'])
      (let [[recons next-id'] (map-children (fn [[child-c child-e] next-id]
                                              (reconcile child-c child-e next-id))
                                            next-id
                                            (map vector c-children new-children))]
        [{:components (map :component recons)
          :updates (mapcat :updates recons)} next-id']))))

(defn reconcile-primitive [{old-elt :component/element
                            node :component/node :as c} elt next-id]
  (if (not= (get-type old-elt) (get-type elt))
    (let [[{:keys [component updates]} next-id'] (build-component elt next-id)]
      [{:component component
        :updates (concat updates
                         [{:update/type :remove
                           :remove/node node}
                          {:update/type :destroy
                           :destroy/node node}])}
       next-id'])
    (let [new-children (get-children elt)
          [{children-reconciled :components 
            children-updates :updates} next-id'] (reconcile-children c new-children next-id)
          old-props (get-props old-elt)
          new-props (get-props elt)]
      [{:component (assoc c
                          :component/element elt
                          :component/children children-reconciled)
        :updates (if (= old-props new-props)
                   children-updates
                   (cons {:update/type :update-props
                          :update-props/node node
                          :update-props/new-props new-props
                          :update-props/old-props old-props}
                         children-updates))}
       next-id'])))

(defn reconcile-user [{c-subst :component/subst
                       old-elt :component/element :as c} {[type & args] :elt key :key :as elt} next-id]
  (if (= elt old-elt)
    [{:component c
      :updates []} next-id]
    (let [subst (apply type args)
          [{new-c-subst :component
            updates :updates} next-id'] (reconcile c-subst {:elt subst
                                                            :key key} next-id)]
      [{:component (assoc c
                          :component/subst new-c-subst
                          :component/element elt
                          :component/node (:component/node new-c-subst))
         :updates updates} next-id'])))

(defn reconcile [c elt next-id]
  (if (user-component? elt)
    (reconcile-user c elt next-id)
    (reconcile-primitive c elt next-id)))

(comment


  (def e0 {:elt [:div {:on-click (fn [])}], :key 0})

  (def e1 {:elt [:div
                 {:on-click (fn [])}
                 [:div
                  {:on-click (fn [])}
                  [:text {:text "0"}]]],
           :key 0})

  (def e2 {:elt [:div
                 {:on-click (fn [])}
                 [:div
                  {:on-click (fn [])}
                  [:text {:text "0"}]]
                 [:div
                  {:on-click (fn [])}
                  [:text {:text "1"}]]],
           :key 0})

  (def c0 (build-component e0 0))
  

  (def r0 (reconcile (:component (first c0)) e1 1))

  (reconcile (:component (first r0)) e2 3)

  (def c1 (build-component e1 0))

  (def r1 (reconcile (:component (first c1)) e2 3))
  

  )
