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

(defn- get-props* [[_ props]]
  (if (map? props) props nil))

(defn get-children [{[_ props & r] :elt}]
  (let [children (cond
                   (map? props) r
                   (some? props) (cons props r)
                   :else r)]
    (persistent! (second (reduce (fn [[indices res] e]
                                   (if-let [key (or (:key (get-props* e)) (:key (meta e)))]
                                     [indices (conj! res {:elt e :key key})]
                                     (let [type (first e)
                                           indices' (update indices type (fn [i] (if i (inc i) 0)))
                                           idx (indices' type)]
                                       [indices' (conj! res {:elt e :key [type idx]})])))
                                 [{} (transient [])] children)))))

(defn get-props [elt-with-key]
  (get-props* (:elt elt-with-key)))

(defn get-type [{[type] :elt}] type)

(defn user-component? [elt]
   (fn? (get-type elt)))

(defn map-children [f ctx children]
  (let [[recons* ctx'] (reduce (fn [[recons ctx] c]
                                     (let [[r ctx'] (f c ctx)]
                                       [(conj! recons r) ctx']))
                                   [(transient []) ctx]
                                   children)]
    [(persistent! recons*) ctx']))

(def ^:dynamic *sink* nil)

(defn build-component [{[type & args] :elt key :key :as elt} ctx]
  (if (user-component? elt)
    (let [sink (atom nil)
          render (type (fn
                      ([] nil)
                      ([x y] (reset! *sink* y) nil)
                      ([x] x)))
          [state subst] (binding [*sink* (atom nil)]
                          [(apply render (render) args) @*sink*])
          [c-subst ctx'] (build-component {:elt subst
                                           :key key} ctx)]
      [{:component/node (:component/node c-subst)
        :component/state state
        :component/render render
        :component/element elt
        :component/subst c-subst}
       ctx'])
    (let [new-node (:next-id ctx)
          [c-components ctx'] (map-children build-component (update ctx :next-id inc) (get-children elt))]
      [{:component/node new-node
        :component/element elt
        :component/children c-components}
       (update ctx' :updates (fn [updates]
                               (transduce
                                (map-indexed (fn [i c]
                                               {:update/type :add
                                                :add/index i
                                                :add/parent new-node
                                                :add/child (:component/node c)}))
                                conj!
                                (conj! updates
                                       {:update/type :make-node
                                        :make-node/node new-node
                                        :make-node/type type
                                        :make-node/props (get-props elt)})
                                c-components)))])))

(declare reconcile)

(defn reconcile-children [{c-children :component/children
                           c-node :component/node :as c} new-children ctx]
  (let [old-keys (map (comp :key :component/element) c-children)
        new-keys (map :key new-children)]
    (if (not= old-keys new-keys)
      (let [common (into #{} (lcs old-keys new-keys))
            key->component (into {}
                                 (map (fn [c] [(:key (:component/element c)) c]))
                                 c-children)
            
            new-keys-set (into #{} new-keys)
            ctx-with-removes (update ctx :updates
                                     (fn [updates]
                                       (transduce (comp (remove #(contains? common (-> % :component/element :key)))
                                                        (mapcat
                                                         (fn [{node :component/node
                                                              elt :component/element}]
                                                           (let [remove {:update/type :remove
                                                                         :remove/node node}]
                                                             (if (contains? new-keys-set (:key elt))
                                                               [remove]
                                                               [remove {:update/type :destroy
                                                                        :destroy/node node}])))))
                                                  conj! updates c-children)))

            [children-reconciled ctx'] (map-children (fn [child ctx]
                                                       (if-let [old-c (key->component (:key child))]
                                                         (reconcile old-c child ctx)
                                                         (build-component child ctx)))
                                                     ctx-with-removes new-children)]
        [children-reconciled
         (update ctx' :updates
                 (fn [updates]
                   (transduce (keep-indexed (fn [i {elt :component/element
                                                   child-node :component/node}]
                                              (when-not (contains? common (:key elt))
                                                {:update/type :add
                                                 :add/index i
                                                 :add/child child-node
                                                 :add/parent c-node})))
                              conj! updates children-reconciled)))])
      
      (map-children (fn [[child-c child-e] ctx]
                      (reconcile child-c child-e ctx))
                    ctx
                    (map vector c-children new-children)))))

(defn reconcile-primitive [{old-elt :component/element
                            node :component/node :as c} elt ctx]
  (if (not= (get-type old-elt) (get-type elt))
    (build-component elt ctx)
    (let [new-children (get-children elt)
          [children-reconciled ctx'] (reconcile-children c new-children ctx)
          old-props (get-props old-elt)
          new-props (get-props elt)]
      [(assoc c
              :component/element elt
              :component/children children-reconciled)
       (cond-> ctx'
         (not= old-props new-props)
         (update :updates conj! {:update/type :update-props
                                 :update-props/node node
                                 :update-props/new-props new-props
                                 :update-props/old-props old-props}))])))

(defn reconcile-user [{c-subst :component/subst
                       render :component/render
                       state :component/state
                       old-elt :component/element :as c} {[_ & args] :elt key :key :as elt} ctx]  
  (let [[state' subst] (binding [*sink* (atom nil)]
                         [(apply render state args)
                          @*sink*])        
        [new-c-subst ctx'] (reconcile c-subst {:elt subst :key key} ctx)]
    [(assoc c
             :component/subst new-c-subst
             :component/state state'
             :component/element elt
             :component/node (:component/node new-c-subst)) ctx']))

(defn reconcile [c elt ctx]
  (if (user-component? elt)
    (reconcile-user c elt ctx)
    (reconcile-primitive c elt ctx)))

(defn pure-component []
  (fn [r-f]
    (fn
      ([] {:foreign (r-f)})
      ([{:keys [foreign props] :as state} & args]
       (if (= args props)
         state
         {:foreign (apply r-f foreign args)
          :props args}))
      ([state] (r-f (:foreign state))))))

(defn component [render]
  (fn [r-f]
    (fn
      ([] (r-f))
      ([state & args]
       (r-f state (apply render args)))
      ([state] (r-f state)))))

(def user-component
  (comp
   (pure-component)
   (component
    (fn [x y]
      [:div {} [:text {:text (str x y)}]]))))



(let [[{c :component} next-id] (build-component {:elt [user-component 1 2]
                                                 :key 0} 0)]
  (reconcile c {:elt [user-component 1 3]
                :key 0} next-id))

(defn my-comp [update!]
  (fn [rf]
    (fn
      ([] {:state 0
           :downstream (rf)})
      ([state x y]
       {:state (+ x y)
        :downstream (rf (:downstream state) [:div [:text {:text (str (+ x y))}]])})
      ([state]
       
       ))))

(def my-comp1 (component (fn [n click]
                           (if (< 0 n)
                             [:div
                              [:text {:text (str n)}]
                              ^{:key 1} [my-comp1 (dec n) click]
                              ^{:key 2} [my-comp1 (dec n) click]]
                             [:div {:on-click click}
                              [:text {:text (str n)}]]))))


(comment

  (def e {:elt [my-comp1 10 "fuck yo"]
          :key 0})

  (def cc (build-component e {:updates (transient [])
                              :next-id 0}))
  (def c (first cc))
  c

  (time
   (do
     (doall (persistent! (:updates (second (reconcile c {:elt [my-comp1 10 "fuck yo"]
                                                         :key 0} {:updates (transient [])
                                                                  :next-id (:next-id (second cc))}))))) 
     nil)
   
   )


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
