(ns flow.bindings
  (:require [flow.compile :refer [compile-form]]
            [clojure.set :as set]
            [flow.protocols :as fp]
            [flow.util :as u]
            [flow.bindings.protocols :as bp]))

(defn destructuring-bind-syms [bind]
  (letfn [(dbs* [bind] (cond
                        (map? bind) (let [{:keys [as]
                                           ks :keys} bind]
                                      (concat (mapcat dbs* ks)
                                              (dbs* as)
                                              (mapcat dbs* (keys (dissoc bind :keys :as)))))
                        (vector? bind) (-> (mapcat dbs* bind)
                                           set
                                           (disj '&))
                        (symbol? bind) [(symbol (name bind))]))]
    (set (dbs* bind))))

(defn compile-bindings [bindings opts]
  (reduce (fn [{:keys [compiled-bindings opts] :as acc} {:keys [bind value path]}]
            (let [compiled-value (compile-form value opts)
                        
                  destructured-syms (destructuring-bind-syms bind)
                  deps (fp/form-deps compiled-value)

                  bind-values->map-sym (symbol (str path "-value->map"))
                  !value-sym (symbol (str "!" path "-value"))
                  value-sym (symbol (str path "-value"))]

              {:compiled-bindings (conj compiled-bindings
                                        (reify bp/CompiledBindings
                                          (value-deps [_] deps)

                                          (bindings [_]
                                            (concat (fp/bindings compiled-value)
                                                    [[!value-sym `(atom nil)]]

                                                    [[bind-values->map-sym `(fn bind-values->map# [value#]
                                                                              (let [~bind value#]
                                                                                ~(->> (for [bind-sym destructured-syms]
                                                                                        `[(quote ~bind-sym) ~bind-sym])
                                                                                      (into {}))))]]))

                                          (destructured-syms [_]
                                            destructured-syms)

                                          (initial-bindings [_ state-sym]
                                            `[[[~value-sym ~(fp/initial-value-form compiled-value state-sym)]]
                                              
                                              [[~state-sym (merge ~state-sym (~bind-values->map-sym ~value-sym))]
                                               
                                               [~'_ (reset! ~!value-sym ~value-sym)]]])

                                          (updated-bindings [_ new-state-sym updated-vars-sym]
                                            `[[[~value-sym ~(u/with-updated-deps-check deps updated-vars-sym
                                                              (fp/updated-value-form compiled-value new-state-sym updated-vars-sym)
                                                              `@~!value-sym)]]
                                              
                                              [[~new-state-sym (merge ~new-state-sym (~bind-values->map-sym ~value-sym))]

                                               [~updated-vars-sym (set/union ~updated-vars-sym
                                                                             (flow.diff/updated-keys (~bind-values->map-sym @~!value-sym)
                                                                                                     (~bind-values->map-sym ~value-sym)))]
                                               [~'_ (reset! ~!value-sym ~value-sym)]]])))
               
               :opts (update-in opts
                                [(if (seq deps)
                                   :dynamic-syms
                                   :local-syms)]
                                set/union destructured-syms)}))
                
          {:compiled-bindings []
           :opts opts}
                
          bindings))

(defn bindings-deps [compiled-bindings compiled-body]
  (reduce (fn [deps-acc compiled-binding]
            (-> deps-acc
                (set/difference (bp/destructured-syms compiled-binding))
                (set/union (bp/value-deps compiled-binding))))
          (fp/form-deps compiled-body)
          (reverse compiled-bindings)))
