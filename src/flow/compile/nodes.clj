(ns flow.compile.nodes
  (:require [flow.compile :refer [compile-form]]
            [flow.util :as u]
            [flow.bindings :as b]))

(alias 'fd (doto 'flow.dom create-ns))
(alias 'fp (doto 'flow.protocols create-ns))

(declare compile-node)

(defn compile-attr [elem-sym [k v] path
                    {:keys [state old-state new-state updated-vars] :as opts}]
  
  (let [{:keys [deps inline-value]} (compile-form v (assoc opts
                                                      :value? true
                                                      :box? false))
        value-sym (symbol (str path "-attrs-" (name k)))]
    {:deps deps

     :el-build `[(fd/set-attr! ~elem-sym ~k ~inline-value)]

     :on-update (when (not-empty deps)
                  `[(when (u/deps-updated? ~(u/quote-deps deps) ~updated-vars)
                      (let [~state ~new-state]
                        (fd/set-attr! ~elem-sym ~k ~inline-value)))])}))

(defn compile-style [elem-sym [k v] path {:keys [state new-state updated-vars] :as opts}]
  (let [{:keys [deps inline-value]} (compile-form v (assoc opts
                                                       :value? true
                                                       :box? false))
        value-sym (symbol (str path "-style-" (name k)))]
    {:deps deps

     :el-build `[(fd/set-style! ~elem-sym ~k ~inline-value)]

     :on-update (when (not-empty deps)
                  `[(when (u/deps-updated? ~(u/quote-deps deps) ~updated-vars)
                      (let [~state ~new-state]
                        (fd/set-style! ~elem-sym ~k ~inline-value)))])}))

(defn compile-classes [elem-sym classes {:keys [state old-state new-state updated-vars] :as opts}]
  (when (seq classes)
    (let [compiled-classes (map #(compile-form % opts) classes)
          deps (set (mapcat :deps compiled-classes))]

      {:deps deps

       :el-build [`(fd/add-classes! ~elem-sym
                                   (-> (set [~@(map :inline-value compiled-classes)])
                                       (disj nil)))]

       :on-update (when (not-empty deps)
                    [`(letfn [(classes-for# [state#]
                                (let [~state state#]
                                  (-> (set [~@(map :inline-value compiled-classes)])
                                      (disj nil))))]
                      
                        (when (u/deps-updated? ~(u/quote-deps deps) ~updated-vars)
                          (fd/update-classes! ~elem-sym
                                              (classes-for# (quote ~old-state))
                                              (classes-for# (quote ~new-state)))))])})))

(defn compile-listener [elem-sym {:keys [event listener]} {:keys [state-sym] :as opts}]
  {:el-build [`(fd/add-listener! ~elem-sym ~event ~(:inline-value (compile-form listener opts)))]})

(defn compile-child [elem-sym child {:keys [state old-state new-state updated-vars] :as opts}]
  (let [{:keys [el] :as compiled-child} (if (= :node (:type child))
                                          (compile-node child opts)
                                          (compile-form child opts))
        child-sym (symbol (:path child))]

    (if el
      (let [{:keys [el deps declarations]} compiled-child]
        {:deps deps
         :declarations declarations
         :el-bindings [[child-sym el]]
         :el-build [`(fd/append-child! ~elem-sym (fp/build ~child-sym ~state))]
         
         :on-update [`(when (fp/should-update? ~child-sym ~updated-vars)
                        (fp/handle-update! ~child-sym ~old-state ~new-state ~updated-vars))]})

      (let [{:keys [el-return]} compiled-child]
        (-> compiled-child
            (update-in [:el-build] conj `(fd/append-child! ~elem-sym ~el-return)))))))

(comment
  (require 'flow.parse)

  (let [syms {:state 'flow-test-state
              :old-state 'flow-test-old-state
              :new-state 'flow-test-new-state
              :updated-vars 'flow-test-updated-vars}]
    (-> (compile-form (flow.parse/parse-form '[:div
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
     
     :el-build `[~@(when id
                    [`(set! (.-id ~elem-sym) ~id)])
                
                ~@(mapcat :el-build compiled-children)
                ~@(mapcat :el-build compiled-attrs)
                ~@(mapcat :el-build compiled-styles)
                ~@(mapcat :el-build compiled-listeners)
                ~@(:el-build compiled-classes)]

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

(defmethod compile-form :node [{:keys [path] :as node}
                               {:keys [state old-state new-state updated-vars] :as opts}]
  (let [{:keys [el-bindings el-return el-build on-update deps declarations]} (compile-node node opts)
        
        el-fn (symbol (str path "-node"))]

    {:el `(~el-fn)
     :deps deps
     :declarations (concat declarations

                           [(let [!bindings (gensym "binds")]
                              `(defn ~el-fn []
                                 (let [~!bindings (atom nil)]
                              
                                   (reify fp/Box
                                     (~'should-update? [_# updated-vars#]
                                       (u/deps-updated? ~(u/quote-deps deps) updated-vars#))
                                
                                     (~'build [_# ~state]
                                       (reset! ~!bindings ~(b/init-bindings el-bindings))

                                       (let [~@(b/read-bindings !bindings el-bindings)]
                                         ~@el-build
                                      
                                         ~el-return))
                                
                                     (~'handle-update! [_# ~old-state ~new-state ~updated-vars]
                                       (let [~@(b/read-bindings !bindings el-bindings)]
                                         ~@on-update))))))])}))

