(ns flow.compile.nodes
  (:require [flow.compile :refer [compile-identity compile-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

(alias 'fd (doto 'flow.dom create-ns))
(alias 'fh (doto 'flow.holder create-ns))
(alias 'fs (doto 'flow.state create-ns))

(declare compile-node)

(defn compile-attr [elem-sym [k v] {:keys [path] :as opts}]
  
  (let [compiled-value (compile-value v opts)
        deps (fp/value-deps compiled-value)]

    {:soft-deps deps
     :initial-form `(fd/set-attr! ~elem-sym ~k ~(fp/inline-value-form compiled-value))
     :update-form (u/with-updated-deps-check deps
                    `(fd/set-attr! ~elem-sym ~k ~(fp/inline-value-form compiled-value)))}))

(defn compile-style [elem-sym [k v] {:keys [path] :as opts}]
  (let [compiled-value (compile-value v opts)
        deps (fp/value-deps compiled-value)]

    {:soft-deps deps
     :initial-form `(fd/set-style! ~elem-sym ~k ~(fp/inline-value-form compiled-value))
     :update-form (u/with-updated-deps-check deps
                    `(fd/set-style! ~elem-sym ~k ~(fp/inline-value-form compiled-value)))}))

(defn compile-classes [elem-sym classes {:keys [path] :as opts}]
  (when (seq classes)
    (let [compiled-classes (map #(compile-value % opts) classes)
          deps (set (mapcat fp/value-deps compiled-classes))]

      {:soft-deps deps
       :initial-form `(fd/set-classes! ~elem-sym (-> [~@(map #(fp/inline-value-form %) compiled-classes)]
                                                     set
                                                     (disj nil)))
       :update-form (u/with-updated-deps-check deps
                      `(fd/set-classes! ~elem-sym (-> [~@(map #(fp/inline-value-form %) compiled-classes)]
                                                      set
                                                      (disj nil))))})))

(defn compile-listener [elem-sym {:keys [event listener]} {:keys [path] :as opts}]
  #_(let [compiled-listener (compile-identity listener opts)
          deps (fp/identity-deps compiled-listener)

          !listener-sym (u/path->sym "!" path event "listener")]
    
      (reify fp/CompiledIdentity
        (identity-deps [_] deps)

        (bindings [_]
          (concat (fp/bindings compiled-listener)
                  (when (seq deps)
                    `[[~!listener-sym (atom nil)]])))

        (initial-form [_ state-sym]
          `(fd/add-listener! ~elem-sym ~event
                             ~(if (seq deps)
                                `(do
                                   (reset! ~!listener-sym ~(fp/initial-form compiled-listener state-sym))
                                   (fn [e#]
                                     ((deref ~!listener-sym) e#)))
                              
                                (fp/initial-form compiled-listener state-sym))))

        (updated-form [_ new-state-sym updated-vars-sym]
          (u/with-updated-deps-check deps updated-vars-sym
            `(reset! ~!listener-sym ~(fp/updated-form compiled-listener new-state-sym updated-vars-sym)))))))

(defn compile-child [elem-sym child {:keys [path] :as opts}]
  (if (= :node (:type child))
    (update-in (compile-node child opts) [:initial-form]
               (fn [form]
                 `(fd/append-child! ~elem-sym ~form)))

    (let [compiled-child (compile-identity child opts)]
      (cond
       (satisfies? fp/CompiledValue compiled-child)
       (let [deps (fp/value-deps compiled-child)
             !current-el (u/path->sym "!current" path "el")]
         
         {:soft-deps deps
          :bindings (when (seq deps)
                      `[[~!current-el (atom nil)]])

          :initial-form (if (seq deps)
                          `(let [initial-holder# (fh/new-element-holder ~(fp/inline-value-form compiled-child))]
                             (reset! ~!current-el initial-holder#)
                             (fh/append-to! initial-holder# ~elem-sym)
                             initial-holder#)
                          
                          `(fd/append-child! ~elem-sym (fd/->node ~(fp/inline-value-form compiled-child))))
          
          :update-form (u/with-updated-deps-check deps
                         `(let [old-holder# @~!current-el
                                new-holder# (fh/new-element-holder ~(fp/inline-value-form compiled-child))]
                            (reset! ~!current-el new-holder#)
                            (fh/swap-child! old-holder# new-holder#)
                            new-holder#))})
       
       (satisfies? fp/CompiledIdentity compiled-child)
       (let [hard-deps (fp/hard-deps compiled-child)
             soft-deps (fp/soft-deps compiled-child)
             deps (set (concat hard-deps soft-deps))
        
             !current-el (u/path->sym "!current" path "el")]

         {:soft-deps deps

          :declarations (fp/declarations compiled-child)

          :bindings `[[~!current-el (atom nil)]]
          
          :initial-form `(let [[initial-value# update-fn#] (fp/build-form compiled-child)
                               initial-holder# (fh/new-element-holder initial-value#)]
                           (reset! ~!current-el [initial-holder# update-fn#])
                           (fh/append-to! initial-holder# ~elem-sym)
                           initial-holder#)

          :update-form (u/with-updated-deps-check deps
                         `(let [[old-holder# update-fn#] @~!current-el
                                [new-holder# update-fn#] (update-fn#)]
                            (reset! ~!current-el [new-holder# update-fn#])
                           
                            (when (u/deps-updated? ~(u/quote-deps hard-deps))
                              (fh/swap-child! old-holder# new-holder#))
                           
                            new-holder#))})))))

(defn compile-node [{:keys [tag id style classes attrs children listeners]} {:keys [path] :as opts}]
  (let [elem-sym (u/path->sym path tag)
        compiled-attrs (map #(compile-attr elem-sym % (u/with-more-path opts [tag])) attrs)
        compiled-styles (map #(compile-style elem-sym % (u/with-more-path opts [tag])) style)
        compiled-classes (compile-classes elem-sym classes (u/with-more-path opts [tag]))
        compiled-children (map #(compile-child elem-sym %1 (u/with-more-path opts [tag (str %2)]))
                               children (range))
        compiled-listeners (map #(compile-listener elem-sym % (u/with-more-path opts [tag])) listeners)

        compiled-parts (concat compiled-children
                               compiled-attrs
                               compiled-styles
                               compiled-listeners
                               [compiled-classes])

        deps (set (mapcat :soft-deps compiled-parts))]
    
    {:soft-deps deps
     :declarations (concat (mapcat :declarations compiled-parts))
     :bindings (concat (mapcat :bindings compiled-parts)
                       `[[~elem-sym (fd/new-element ~tag)]])
     :initial-form `(do
                      ~@(when id
                          [`(set! (.-id ~elem-sym) ~id)])
                      ~@(map :initial-form compiled-parts)
                      ~elem-sym)
     
     :update-form (u/with-updated-deps-check deps
                    `(do
                       ~@(map :update-form compiled-parts)
                       ~elem-sym)
                    
                    elem-sym)}))

(defmethod compile-identity :node [{:keys [tag] :as node} {:keys [path] :as opts}]
  (let [compiled-node (compile-node node opts)
        build-node-sym (u/path->sym "build" path tag)]
    (reify fp/CompiledIdentity
      (hard-deps [_] nil)
      (soft-deps [_] (:soft-deps compiled-node))

      (declarations [_]
        (concat (:declarations compiled-node)
                `[(defn ~build-node-sym []
                    (let [~@(->> (:bindings compiled-node)
                                 (apply concat))]
                      (letfn [(update-node!# []
                                [~(:update-form compiled-node) update-node!#])]
                        [~(:initial-form compiled-node) update-node!#])))]))

      (build-form [_] `(~build-node-sym)))))

