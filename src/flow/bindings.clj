(ns flow.bindings
  (:require [flow.bindings.protocols :as bp]
            [flow.compile :refer [compile-value]]
            [flow.protocols :as fp]
            [flow.util :as u]
            [clojure.set :as set]))

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

#_(defn key-fn [value value-sym]
    (let [manual-key-fn (:flow.core/key-fn (meta value))]
      `(or ~@(when manual-key-fn
               `[(~manual-key-fn ~value-sym)])
           (:flow.core/id ~value-sym)
           (:id ~value-sym)
           ~value-sym)))

#_(defn compile-bindings [bindings {:keys [path] :as opts}]
    (reduce (fn [{:keys [compiled-bindings opts] :as acc} [{:keys [bind value]} idx]]
              (let [binding-path (u/path->sym path "binding" (str idx))
                    compiled-value (compile-value value opts)
                        
                    destructured-syms (destructuring-bind-syms bind)
                    deps (fp/value-deps compiled-value)

                    bind-values->map-sym (u/path->sym binding-path "value->map")
                    !value-sym (u/path->sym "!" binding-path "value")
                    key-sym (u/path->sym binding-path "key")
                    value-sym (u/path->sym binding-path "value")]

                {:compiled-bindings (conj compiled-bindings
                                          (reify bp/CompiledBindings
                                            (value-deps [_] deps)

                                            (bindings [_]
                                              [[!value-sym `(atom nil)]
                                               [bind-values->map-sym `(fn bind-values->map# [value#]
                                                                        (let [~bind value#]
                                                                          ~(->> (for [bind-sym destructured-syms]
                                                                                  `[(quote ~bind-sym) ~bind-sym])
                                                                                (into {}))))]])

                                            (destructured-syms [_]
                                              destructured-syms)

                                            (value-key [_]
                                              key-sym)

                                            (initial-bindings [_ state-sym]
                                              `[[[~value-sym ~(fp/inline-value-form compiled-value state-sym)]]
                                              
                                                [[~key-sym ~(key-fn value value-sym)]

                                                 [~state-sym (merge ~state-sym (~bind-values->map-sym ~value-sym))]
                                               
                                                 [~'_ (reset! ~!value-sym ~value-sym)]]])

                                            (updated-bindings [_ new-state-sym updated-vars-sym]
                                              `[[[~value-sym ~(u/with-updated-deps-check deps updated-vars-sym
                                                                (fp/inline-value-form compiled-value new-state-sym)
                                                                `@~!value-sym)]]
                                              
                                                [[~key-sym ~(key-fn value value-sym)]

                                                 [~new-state-sym (merge ~new-state-sym (~bind-values->map-sym ~value-sym))]

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
                
            (map vector bindings (range))))

#_(defn bindings-deps [compiled-bindings compiled-body]
    (reduce (fn [deps-acc compiled-binding]
              (-> deps-acc
                  (set/difference (bp/destructured-syms compiled-binding))
                  (set/union (bp/value-deps compiled-binding))))
            (fp/identity-deps compiled-body)
            (reverse compiled-bindings)))
