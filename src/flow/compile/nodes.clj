(ns flow.compile.nodes
  (:require [flow.compile :refer [compile-form compile-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

(alias 'fd (doto 'flow.dom create-ns))
(alias 'fh (doto 'flow.holder create-ns))

(declare compile-node)

(defn compile-attr [elem-sym [k v] opts]
  
  (let [compiled-value (compile-value v opts)
        deps (fp/value-deps compiled-value)

        set-attr! (u/path->sym "set" (concat path) "attr!")]
    
    (reify fp/CompiledForm
      (form-deps [_] deps)

      (bindings [_]
        (let [state-sym (gensym "state")]
          `[[~set-attr! (fn [~state-sym]
                          `(fd/set-attr! ~elem-sym ~k ~(fp/inline-value compiled-value state-sym)))]]))

      (initial-value-form [_ state-sym]
        `(~set-attr! ~state-sym))

      (updated-value-form [_ new-state-sym updated-vars-sym]
        (u/with-updated-deps-check deps updated-vars-sym
          `(~set-attr! ~state-sym))))))

(defn compile-style [elem-sym [k v] {:keys [path] :as opts}]
  (let [compiled-value (compile-value v opts)
        deps (fp/value-deps compiled-value)

        set-style! (u/path->sym "set" (concat path) "style!")]
    
    (reify fp/CompiledForm
      (form-deps [_] deps)

      (bindings [_]
        (let [state-sym (gensym "state")]
          `[[~set-style! (fn [~state-sym]
                           `(fd/set-style! ~elem-sym ~k ~(fp/inline-value compiled-value state-sym)))]]))

      (initial-value-form [_ state-sym]
        `(~set-style! ~state-sym))

      (updated-value-form [_ new-state-sym updated-vars-sym]
        (u/with-updated-deps-check deps updated-vars-sym
          `(~set-style! ~state-sym))))))

(defn compile-classes [elem-sym classes {:keys [path] :as opts}]
  (when (seq classes)
    (let [compiled-classes (map #(compile-value % opts) classes)
          deps (set (mapcat fp/value-deps compiled-classes))

          set-classes! (u/path->sym "set" (concat path) "classes!")]

      (reify fp/CompiledForm
        (form-deps [_] deps)

        (bindings [_]
          (let [state-sym (gensym "state")]
            `[[~set-classes! (fn [~state-sym]
                               (fd/set-classes! ~elem-sym (-> [~@(map #(fp/inline-value % state-sym) compiled-classes)]
                                                              set
                                                              (disj nil))))]]))

        (initial-value-form [_ state-sym]
          `(~set-classes! ~state-sym))

        (updated-value-form [_ new-state-sym updated-vars-sym]
          (u/with-updated-deps-check deps updated-vars-sym
            `(~set-classes! ~new-state-sym)))))))

(defn compile-listener [elem-sym {:keys [event listener]} {:keys [path] :as opts}]
  (let [compiled-listener (compile-value listener opts)
        deps (fp/value-deps compiled-listener)

        !listener-sym (u/path->sym "!" path event "listener")]
    
    (reify fp/CompiledForm
      (form-deps [_] deps)

      (bindings [_]
        (when (seq deps)
          `[[~!listener-sym (atom nil)]]))

      (initial-value-form [_ state-sym]
        `(fd/add-listener! ~elem-sym ~event
                           ~(if (seq deps)
                              `(fn [e#]
                                 ((deref ~!listener-sym) e#))
                              (fp/inline-value compiled-listener state-sym))))

      (updated-value-form [_ new-state-sym updated-vars-sym]
        (u/with-updated-deps-check deps updated-vars-sym
          `(reset! ~!listener-sym ~(fp/inline-value compiled-listener new-state-sym)))))))

(defn compile-child [elem-sym {:keys [path] :as child} opts]
  (let [compiled-child (compile-form child opts)
        deps (fp/form-deps compiled-child)
        
        !holder (symbol (str "!" path "-child"))]

    (reify fp/CompiledForm
      (form-deps [_]
        deps)

      (bindings [_]
        (concat (fp/bindings compiled-child)
                [[!holder `(atom nil)]]))

      (initial-value-form [_ state-sym]
        `(let [initial-holder# (fh/new-element-holder ~(fp/initial-value-form compiled-child state-sym))]
           (reset! ~!holder initial-holder#)
           (fh/append-to! initial-holder# ~elem-sym)))

      (updated-value-form [_ new-state-sym updated-vars-sym]
        (u/with-updated-deps-check deps updated-vars-sym
          `(let [new-child# ~(fp/updated-value-form compiled-child
                                                    new-state-sym
                                                    updated-vars-sym)]
             
             (reset! ~!holder (fh/swap-child! @~!holder new-child#))))))))

(comment
  (require 'flow.parse)

  (let [syms {:state 'flow-test-state
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

      (updated-value-form [_ new-state-sym updated-vars-sym]
        (u/with-updated-deps-check deps updated-vars-sym
          `(do
             ~@(map #(fp/updated-value-form % new-state-sym updated-vars-sym)
                    compiled-parts)
             
             ~elem-sym)
          elem-sym)))))

