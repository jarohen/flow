(ns flow.compile.elements.calls
  (:require [flow.compile :refer [compile-el compile-value]]
            [flow.bindings :as b]
            [flow.util :as u]
            [clojure.set :as set]))

(alias 'f (doto 'flow.core create-ns))
(alias 'fp (doto 'flow.protocols create-ns))

(defmulti compile-call
  (fn [call opts]
    (:call-type call)))

(defmethod compile-call :do [{:keys [path side-effects return]} opts]
  (let [do-el (symbol path)
        compiled-return (compile-el return opts)
        deps (:deps compiled-return)]

    (if (empty? side-effects)
      compiled-return

      {:el `(~do-el)
       :deps deps
       :declarations (concat (:declarations compiled-return)

                             [`(defn ~do-el []
                                 (let [downstream-el# ~(:el compiled-return)]
                                   
                                   (reify fp/DynamicElement
                                     (~'should-update? [_# updated-vars#]
                                       (fp/should-update? downstream-el# updated-vars#))

                                     (~'build-element [_# state#]
                                       ~@side-effects
                                       (fp/build-element downstream-el#))

                                     (~'handle-update! [_# old-state# new-state# updated-vars#]
                                       (fp/handle-update! downstream-el# old-state# new-state# updated-vars#)))))])})))

(defmethod compile-call :if [{:keys [path test then else]} opts]
  (let [if-sym (symbol path)
        compiled-test (compile-value test opts)
        [compiled-then compiled-else] (map #(compile-el % opts) [then else])
        deps (mapcat :deps [compiled-test compiled-then compiled-else])]

    {:el `(~if-sym)
     :deps deps
     :declarations (concat (mapcat :declarations [compiled-test compiled-then compiled-else])
                           [`(defn ~if-sym []
                               (flow.if/if->el ~(u/quote-deps deps)
                                               ~(:value compiled-test)
                                               ~(:el compiled-then)
                                               ~(:el compiled-else)))])}))

(defn with-bind-values->map-fn [{:keys [bind destructured-binds] :as compiled-el}]
  (cond-> compiled-el
    (not (keyword? bind)) (assoc :bind-values->map `(fn bind-values-map# [value#]
                                                      (let [~bind value#]
                                                        ~(->> (for [bind-sym destructured-binds]
                                                                `[(quote ~bind-sym) ~bind-sym])
                                                              (into {})))))))

(defmethod compile-call :let [{:keys [bindings body]} opts]
  (if (empty? bindings)
    (compile-el body opts)
    
    (let [[{:keys [bind value path]} & more-bindings] bindings

          destructured-binds (b/destructuring-bind-syms bind)
          
          compiled-value (-> (compile-value value opts)
                             (assoc :bind bind
                                    :destructured-binds destructured-binds)
                             with-bind-values->map-fn)
          
          dynamic-value? (empty? (:deps compiled-value))
          
          compiled-body (compile-call {:call-type :let
                                       :bindings more-bindings
                                       :body body}

                                      (update-in opts
                                                 [(if dynamic-value?
                                                    :local-syms
                                                    :dynamic-syms)]
                                                 (comp set #(concat % destructured-binds))))

          deps (-> (set (:deps compiled-body))
                   (set/difference destructured-binds)
                   (concat (:deps compiled-value))
                   set)

          let-sym (symbol path)]

      {:el `(~let-sym)
       :deps deps
       :declarations (concat (mapcat :declarations [compiled-value compiled-body])
                             [`(defn ~let-sym []
                                 (flow.let/let->el ~(:value compiled-value)
                                                   ~(:el compiled-body)

                                                   ~(u/quote-deps deps)

                                                   #{~@(for [bind-sym destructured-binds]
                                                         `(quote ~bind-sym))}

                                                   ~(:bind-values->map compiled-value)))])})))

(defn parse-for-bindings [bindings {:keys [dynamic-syms local-syms] :as opts}]
  (reduce (fn [{:keys [compiled-values dynamic-syms local-syms] :as acc} {:keys [bind value path]}]
            (let [{:keys [deps] :as compiled-value} (compile-value value (assoc opts
                                                                           :dynamic-syms dynamic-syms
                                                                           :local-syms local-syms))
                  destructured-binds (b/destructuring-bind-syms bind)]
                                              
              (-> acc
                  (update-in [(if (empty? deps) :local-syms :dynamic-syms)] set/union destructured-binds)
                  (update-in [:compiled-values] conj (-> compiled-value
                                                         (assoc :bind bind
                                                                :destructured-binds destructured-binds)
                                                         with-bind-values->map-fn)))))
                                                                  
          {:dynamic-syms dynamic-syms
           :local-syms local-syms
           :compiled-values []}
                
          bindings))

(defn for-deps [compiled-values compiled-body]
  (reduce (fn [deps-acc {:keys [deps destructured-binds]}]
            (-> deps-acc
                (set/difference destructured-binds)
                (set/union deps)))
          (:deps compiled-body)
          (reverse compiled-values)))



(comment
  (require 'flow.parse)
  
  (compile-el (flow.parse/parse-form '(for [x (<<! !color)
                                            y (range 4)]
                                        [:div x y])
                                     {:elem? true
                                      :path "flow-test"})
              {})
  )

(defmethod compile-call :for [{:keys [bindings body path]} opts]
  (let [{:keys [compiled-values dynamic-syms local-syms]} (parse-for-bindings bindings opts)

        compiled-body (compile-el body (assoc opts
                                         :dynamic-syms dynamic-syms
                                         :local-syms local-syms))

        deps (for-deps compiled-values compiled-body)

        for-sym (symbol path)]

    {:el `(~for-sym)
     :deps deps
     :declarations (concat (mapcat :declarations (concat compiled-values [compiled-body]))
                           
                           [`(defn ~for-sym []
                               ~(let [dynamic-value-syms (for [idx (range (count compiled-values))]
                                                           (symbol (str for-sym "-dynamic-values-" idx)))
                                      bind-values-map-syms (for [idx (range (count compiled-values))]
                                                             (symbol (str for-sym "-bind-values-map-" idx)))]
                              
                                  `(let [~@(mapcat (fn [dynamic-value-sym bind-values-map-sym {:keys [value bind-values->map]}]
                                                     [dynamic-value-sym value
                                                      bind-values-map-sym bind-values->map])
                                                   dynamic-value-syms bind-values-map-syms compiled-values)]
                                     
                                     (flow.for/for->el ~(u/quote-deps deps)

                                                       ~(let [state (symbol (str for-sym "-state"))]
                                                          `(fn for-values# [~state]
                                                             ~(let [values (map #(symbol (str for-sym "-value-" %)) (range (count bindings)))]
                                                                `(for [~@(mapcat (fn [value dynamic-value-sym bind-values-map-sym]
                                                                                   `[~value (fp/current-value ~dynamic-value-sym ~state)
                                                                                     :let [~state (merge ~state (~bind-values-map-sym ~value))]])
                                                                                 values dynamic-value-syms bind-values-map-syms)]
                                                                   {:values [~@values]
                                                                    :keys (map (fn [key-fn# value#]
                                                                                 (or (when key-fn#
                                                                                       (key-fn# value#))
                                                                                     (::f/id value#)
                                                                                     (:id value#)
                                                                                     value#))
                                                                               [~@(map :key-fn compiled-values)]
                                                                               [~@values])
                                                                    :state ~state}))))
                                                       
                                                       ~(:el compiled-body)))))])})

  )

(defmethod compile-call :fn-call [{:keys [args]} opts]
  ;; TODO
  )

(defmethod compile-call :unwrap-cursor [{:keys [cursor path]} opts]
  (let [el (symbol (str path))
        deps #{cursor}]
    
    {:el `(flow.cursors/cursor->el ~(u/quote-deps deps) (quote ~cursor))
     :deps #{cursor}}))

(defmethod compile-call :wrap-cursor [{:keys [cursor]} opts]
  ;; TODO 
  )

(defmethod compile-el :call [call opts]
  (compile-call call opts))
