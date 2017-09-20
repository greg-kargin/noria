(ns server-ws.noria)

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
                                      :remove/child node}]
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
                               :remove/child node}
                              {:update/type :destroy
                               :destroy/node node}])])
    (let [[children-reconciled children-updates] (reconcile-children c elt tk)
          [old-props new-props] [(get-props old-e) (get-props elt)]
          props-updates (when (not= old-props new-props)
                          [{:update/type :update-props
                            :update-props/node node
                            :update-props/old-props old-props
                            :update-props/new-props new-props}])]
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
        (perform-updates [tk u]))))

  (def tk (tt))

  (def comp (first (build-component {:elt [label 1]
                                     :key 0} tk)))

  (def comp' (first (reconcile comp {:elt [label 3] :key 0} tk)))

  comp'

  (def comp'' (reconcile comp' {:elt [labels 2] :key 0} tk))

  comp''

  )
