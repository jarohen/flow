(ns flow.compile.nodes
  (:require [flow.compile :refer [compile-form]]
            [flow.util :as u]
            [flow.bindings :as b]
            [flow.protocols :as fp]))

(alias 'fd (doto 'flow.dom create-ns))

(declare compile-node)

(defn compile-attr [elem-sym [k v] path
                    {:keys [state old-state new-state updated-vars] :as opts}]
  
  (let [compiled-value (compile-form v opts)
        deps (fp/form-deps compiled-value)]
    
    (reify fp/CompiledForm
      (form-deps [_] deps)

      (bindings [_] (fp/bindings compiled-value))

      (initial-value-form [_ state-sym]
        `(fd/set-attr! ~elem-sym ~k ~(fp/initial-value-form compiled-value state-sym)))

      (updated-value-form [_ old-state-sym new-state-sym updated-vars-sym]
        (u/with-updated-deps-check deps updated-vars-sym
          `(when-let [[old-value# new-value#] ~(fp/updated-value-form compiled-value
                                                                      old-state-sym
                                                                      new-state-sym
                                                                      updated-vars-sym)]
             (when-not (= old-value# new-value#)
               (fd/set-attr! ~elem-sym ~k new-value#))))))))

(defn compile-style [elem-sym [k v] path opts]
  (let [compiled-value (compile-form v opts)
        deps (fp/form-deps compiled-value)]
    
    (reify fp/CompiledForm
      (form-deps [_] deps)

      (bindings [_] (fp/bindings compiled-value))

      (initial-value-form [_ state-sym]
        `(fd/set-style! ~elem-sym ~k ~(fp/initial-value-form compiled-value state-sym)))

      (updated-value-form [_ old-state-sym new-state-sym updated-vars-sym]
        (u/with-updated-deps-check deps updated-vars-sym
          `(when-let [[old-value# new-value#] ~(fp/updated-value-form compiled-value
                                                                      old-state-sym
                                                                      new-state-sym
                                                                      updated-vars-sym)]
             (when-not (= old-value# new-value#)
               (fd/set-style! ~elem-sym ~k new-value#))))))))

(defn compile-classes [elem-sym classes opts]
  (when (seq classes)
    (let [compiled-classes (map #(compile-form % opts) classes)
          deps (set (mapcat fp/form-deps compiled-classes))]

      (reify fp/CompiledForm
        (form-deps [_] deps)

        (bindings [_]
          (map fp/bindings compiled-classes))

        (initial-value-form [_ state-sym]
          `(fd/add-classes! ~elem-sym
                            (-> (set [~@(map #(fp/initial-value-form % state-sym)
                                             compiled-classes)])
                                (disj nil))))

        (updated-value-form [_ old-state-sym new-state-sym updated-vars-sym]
          (u/with-updated-deps-check deps updated-vars-sym
            `(let [changed-classes# [~@(map #(fp/updated-value-form % old-state-sym new-state-sym updated-vars-sym) compiled-classes)]
                   old-classes# (-> (map first changed-classes#)
                                    set
                                    (disj nil))
                   new-classes# (-> (map second changed-classes#)
                                    set
                                    (disj nil))]
               (when-not (= old-classes# new-classes#)
                 (fd/update-classes! ~elem-sym old-classes# new-classes#)))))))))

(defn compile-listener [elem-sym {:keys [event listener path]} opts]
  (let [compiled-listener (compile-form listener opts)
        deps (fp/form-deps compiled-listener)

        !listener-sym (symbol (str "!" path "-" (name event) "-listener"))]
    
    (reify fp/CompiledForm
      (form-deps [_] deps)

      (bindings [_]
        (concat (fp/bindings compiled-listener)
                (when (seq deps)
                  `[[~!listener-sym (atom nil)]])))

      (initial-value-form [_ state-sym]
        `(fd/add-listener! ~elem-sym ~event
                           ~(if (seq deps)
                              `(fn [e#]
                                 ((deref ~!listener-sym) e#))
                              (fp/initial-value-form compiled-listener state-sym))))

      (updated-value-form [_ old-state-sym new-state-sym updated-vars-sym]
        (u/with-updated-deps-check deps updated-vars-sym
          `(when-let [[old-value# new-value#] ~(fp/updated-value-form compiled-listener
                                                                      old-state-sym
                                                                      new-state-sym
                                                                      updated-vars-sym)]
             (reset! ~!listener-sym new-value#)))))))

(defn compile-child [elem-sym {:keys [path] :as child} opts]
  (let [compiled-child (compile-form child opts)
        deps (fp/form-deps compiled-child)
        
        !child-sym (symbol (str "!" path "-child"))]

    (reify fp/CompiledForm
      (form-deps [_]
        deps)

      (bindings [_]
        (concat (fp/bindings compiled-child)
                [[!child-sym `(atom nil)]]))

      (initial-value-form [_ state-sym]
        `(let [$initial-child# (fd/->node ~(fp/initial-value-form compiled-child state-sym))]
           (reset! ~!child-sym $initial-child#)
           (fd/append-child! ~elem-sym $initial-child#)
           $initial-child#))

      (updated-value-form [_ old-state-sym new-state-sym updated-vars-sym]
        (u/with-updated-deps-check deps updated-vars-sym
          `(let [new-child# ~(fp/updated-value-form compiled-child
                                                    old-state-sym
                                                    new-state-sym
                                                    updated-vars-sym)
                 $old-child# @~!child-sym
                 $new-child# (fd/->node new-child#)]
             
             (when-not (= $old-child# $new-child#)
               (fd/swap-elem! @~!child-sym $new-child#)
               (reset! ~!child-sym $new-child#)
               $new-child#)))))))

(comment
  (require 'flow.parse)

  (let [syms {:state 'flow-test-state
              :old-state 'flow-test-old-state
              :new-state 'flow-test-new-state
              :updated-vars 'flow-test-updated-vars}]
    (-> (compile-form (flow.parse/parse-form '[:div
                                               [:h1 "Show heading is:" (pr-str (!<< !show-heading))]]
                                             {:elem? true
                                              :path "flow-test"})
                      syms)
        #_(render-el syms)))
  )

(defmethod compile-form :node [{:keys [tag id style classes attrs children listeners path]} opts]
  (let [elem-sym (symbol path)
        compiled-attrs (map #(compile-attr elem-sym % path opts) attrs)
        compiled-styles (map #(compile-style elem-sym % path opts) style)
        compiled-classes (compile-classes elem-sym classes opts)
        compiled-children (map #(compile-child elem-sym % opts) children)
        compiled-listeners (map #(compile-listener elem-sym % opts) listeners)

        compiled-parts (concat compiled-children
                               compiled-attrs
                               compiled-styles
                               compiled-listeners
                               [compiled-classes])

        deps (set (mapcat fp/form-deps compiled-parts))]

    (reify fp/CompiledForm
      (form-deps [_]
        deps)

      (bindings [_]
        `[[~elem-sym (fd/new-element ~tag)]
          ~@(mapcat fp/bindings compiled-parts)])

      (initial-value-form [_ state-sym]
        `(do
           ~@(when id
               [`(set! (.-id ~elem-sym) ~id)])
           
           ~@(map #(fp/initial-value-form % state-sym) compiled-parts)

           ~elem-sym))

      (updated-value-form [_ old-state-sym new-state-sym updated-vars-sym]
        (u/with-updated-deps-check deps updated-vars-sym
          `(do
             ~@(map #(fp/updated-value-form % old-state-sym new-state-sym updated-vars-sym)
                    compiled-parts)
             
             ~elem-sym))))))

