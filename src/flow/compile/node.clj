(ns flow.compile.node
  (:require [flow.compile :refer [compile-el compile-value]]
            [flow.util :as u]
            [flow.bindings :as b]))

(alias 'fd (doto 'flow.dom create-ns))
(alias 'fp (doto 'flow.protocols create-ns))

(declare compile-node)

(defn compile-attr [elem-sym [k v] path
                    {:keys [state old-state new-state updated-vars] :as opts}]
  
  (let [{:keys [deps inline-value value declarations]} (compile-value v opts)
        value-sym (symbol (str path "-attrs-" (name k)))]
    {:deps deps

     :el-bindings (when (not-empty deps)
                    [[value-sym value]])
     
     :el-init (if (empty? deps)
                `[(fd/set-attr! ~elem-sym ~k ~inline-value)]
                `[(fd/set-attr! ~elem-sym ~k (fp/current-value ~value-sym ~state))])

     :declarations declarations
     
     :on-update (when (not-empty deps)
                  `[(when (fp/should-update-value? ~value-sym ~updated-vars)
                      (fd/set-attr! ~elem-sym ~k (fp/current-value ~value-sym ~new-state)))])}))

(defn compile-style [elem-sym [k v] path {:keys [state new-state updated-vars] :as opts}]
  (let [{:keys [deps value inline-value declarations]} (compile-value v opts)
        value-sym (symbol (str path "-style-" (name k)))]
    {:deps deps

     :el-bindings (when (not-empty deps)
                    [[value-sym value]])
     
     :el-init (if (empty? deps)
                `[(fd/set-style! ~elem-sym ~k ~inline-value)]
                `[(fd/set-style! ~elem-sym ~k (fp/current-value ~value-sym ~state))])

     :declarations declarations
     
     :on-update (when (not-empty deps)
                  `[(when (fp/should-update-value? ~value-sym ~updated-vars)
                      (fd/set-style! ~elem-sym ~k (fp/current-value ~value-sym ~new-state)))])}))

(defn compile-classes [elem-sym classes {:keys [state old-state new-state] :as opts}]
  (when (seq classes)
    (let [compiled-classes (map #(-> %
                                     (compile-value opts)
                                     (assoc :value-sym (symbol (:path %))))
                                classes)
          deps (set (mapcat :deps compiled-classes))]

      {:deps deps

       :el-bindings (->> compiled-classes
                         (filter (comp not-empty :deps))
                         (map (juxt :value-sym :value)))
       
       :el-init [`(fd/add-classes! ~elem-sym
                                   (-> (set [~@(for [{:keys [value-sym inline-value deps]} compiled-classes]
                                                 (if (empty? deps)
                                                   inline-value
                                                   `(fp/current-value ~value-sym ~state)))])
                                       (disj nil)))]

       :declarations (mapcat :declarations compiled-classes)
       
       :on-update (when (not-empty deps)
                    (letfn [(classes-for [deps-state-sym]
                              `(-> (concat (map #(fp/current-value % ~deps-state-sym)
                                                [~@(->> compiled-classes
                                                        (filter (comp not-empty :deps))
                                                        (map :value-sym))])
                                           
                                           [~@(->> compiled-classes
                                                   (filter (comp empty? :deps))
                                                   (map :inline-value))])
                                   set
                                   (disj nil)))]
                      
                      [`(fd/update-classes! ~elem-sym
                                            ~(classes-for old-state)
                                            ~(classes-for new-state))]))})))

(defn compile-listener [elem-sym {:keys [event listener]} {:keys [state-sym] :as opts}]
  {:el-init [`(fd/add-listener! ~elem-sym ~event ~(:inline-value (compile-value listener opts)))]})

(defn compile-child [elem-sym child {:keys [state old-state new-state updated-vars] :as opts}]
  (let [{:keys [el] :as compiled-child} (if (= :node (:type child))
                                          (compile-node child opts)
                                          (compile-el child opts))
        child-sym (symbol (:path child))]

    (if el
      (let [{:keys [el deps declarations]} compiled-child]
        {:deps deps
         :declarations declarations
         :el-bindings [[child-sym el]]
         :el-init [`(fd/append-child! ~elem-sym (fp/build-element ~child-sym ~state))]
         
         :on-update [`(when (fp/should-update-el? ~child-sym ~updated-vars)
                        (fp/handle-update! ~child-sym ~old-state ~new-state ~updated-vars))]})

      (let [{:keys [el-return]} compiled-child]
        (-> compiled-child
            (update-in [:el-init] conj `(fd/append-child! ~elem-sym ~el-return)))))))

(comment
  (require 'flow.parse)

  (let [syms {:state 'flow-test-state
              :old-state 'flow-test-old-state
              :new-state 'flow-test-new-state
              :updated-vars 'flow-test-updated-vars}]
    (-> (compile-el (flow.parse/parse-form '[:div
                                             [:h1 "Show heading is:" (pr-str (<<! !show-heading))]]
                                           {:elem? true
                                            :path "flow-test"})
                    syms)
        #_(render-el syms)))
  )

(defn compile-node [{:keys [tag id style classes attrs children listeners path]} opts]
  (let [elem-sym (gensym "elem")
        compiled-attrs (map #(compile-attr elem-sym % path opts) attrs)
        compiled-styles (map #(compile-style elem-sym % path opts) style)
        compiled-classes (compile-classes elem-sym classes opts)
        compiled-children (map #(compile-child elem-sym % opts) children)
        compiled-listeners (map #(compile-listener elem-sym % opts) listeners)]

    {:el-bindings `[[~elem-sym (fd/new-element ~tag)]
                    ~@(mapcat :el-bindings compiled-attrs)
                    ~@(mapcat :el-bindings compiled-styles)
                    ~@(mapcat :el-bindings compiled-listeners)
                    ~@(:el-bindings compiled-classes)
                    ~@(mapcat :el-bindings compiled-children)]

     :el-return elem-sym
     
     :el-init `[~@(when id
                    [`(set! (.-id ~elem-sym) ~id)])
                
                ~@(mapcat :el-init compiled-attrs)
                ~@(mapcat :el-init compiled-styles)
                ~@(mapcat :el-init compiled-listeners)
                ~@(:el-init compiled-classes)
                ~@(mapcat :el-init compiled-children)]

     :deps (set (concat (mapcat :deps compiled-attrs)
                        (mapcat :deps compiled-styles)
                        (:deps compiled-classes)
                        (mapcat :deps compiled-children)))

     :declarations (concat (mapcat :declarations compiled-attrs)
                           (mapcat :declarations compiled-styles)
                           (:declarations compiled-classes)
                           (mapcat :declarations compiled-children))

     :on-update (concat (mapcat :on-update compiled-attrs)
                        (mapcat :on-update compiled-styles)
                        (:on-update compiled-classes)
                        (mapcat :on-update compiled-children))}))

(defmethod compile-el :node [{:keys [path] :as node}
                             {:keys [state old-state new-state updated-vars] :as opts}]
  (let [{:keys [el-bindings el-return el-init on-update deps declarations]} (compile-node node opts)
        
        el-fn (symbol (str path "-node"))]
    declarations
    {:el `(~el-fn)
     :deps deps
     :declarations (concat declarations

                           [(let [!bindings (gensym "binds")]
                              `(defn ~el-fn []
                                 (let [~!bindings (atom nil)]
                              
                                   (reify fp/DynamicElement
                                     (~'should-update-el? [_# ~updated-vars]
                                       ~(u/deps->should-update deps updated-vars))
                                
                                     (~'build-element [_# ~state]
                                       (reset! ~!bindings ~(b/init-bindings el-bindings))

                                       (let [~@(b/read-bindings !bindings el-bindings)]
                                         ~@el-init
                                      
                                         ~el-return))
                                
                                     (~'handle-update! [_# ~old-state ~new-state ~updated-vars]
                                       (let [~@(b/read-bindings !bindings el-bindings)]
                                         ~@on-update))))))])}))

