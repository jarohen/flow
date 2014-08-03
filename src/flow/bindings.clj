(ns flow.bindings
  (:require [flow.compile :refer [compile-identity]]
            [flow.util :as u]
            [flow.protocols :as fp]
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

>#_(defn key-fn [value value-sym]
    (let [manual-key-fn (:flow.core/key-fn (meta value))]
      `(or ~@(when manual-key-fn
               `[(~manual-key-fn ~value-sym)])
           (:flow.core/id ~value-sym)
           (:id ~value-sym)
           ~value-sym)))

(defn compile-binding [{:keys [bind value idx]} opts]
  (let [compiled-identity (compile-identity value (u/with-more-path opts [(str idx)]))
        
        hard-deps (fp/hard-deps compiled-identity)
        soft-deps (fp/soft-deps compiled-identity)

        destructured-syms (destructuring-bind-syms bind)]

    {:compiled-binding {:hard-deps hard-deps
                        :soft-deps soft-deps

                        :declarations (fp/declarations compiled-identity)
                        :bound-syms destructured-syms

                        :manual-key-fn (:flow.core/key-fn (meta value))

                        :value->state `(fn [~bind]
                                         ~(->> (for [bind-sym destructured-syms]
                                                 `[(quote ~bind-sym) ~bind-sym])
                                               (into {})))
                        :build-binding `(fn []
                                          ~(fp/build-form compiled-identity))}

     :opts (update-in opts
                      [(if (seq hard-deps)
                         :dynamic-syms
                         :local-syms)]
                      set/union destructured-syms)}))

(defn compile-bindings [bindings opts]
  (reduce (fn [{:keys [compiled-bindings opts] :as acc} binding]
            (let [{:keys [compiled-binding opts]} (compile-binding binding opts)]

              {:compiled-bindings (conj compiled-bindings
                                        compiled-binding)
               
               :opts opts}))
                
          {:compiled-bindings []
           :opts opts}
                
          bindings))

(defn bindings-deps [compiled-bindings compiled-body]
  (reduce (fn [{:keys [hard-deps soft-deps] :as deps-acc} {:keys [bound-syms] :as compiled-binding}]
            (-> deps-acc
                (update-in [:hard-deps] set/difference (:bound-syms compiled-binding))
                (update-in [:soft-deps] set/difference (:bound-syms compiled-binding))
                (update-in [:hard-deps] set/union (when (seq (set/intersection bound-syms hard-deps))
                                                    (:hard-deps compiled-binding)))
                (update-in [:soft-deps] set/union (when (seq (set/intersection bound-syms (set/union hard-deps soft-deps)))
                                                    (concat (:soft-deps compiled-binding)
                                                            (:hard-deps compiled-binding))))))

          {:hard-deps (fp/hard-deps compiled-body)
           :soft-deps (fp/soft-deps compiled-body)}
          
          (reverse compiled-bindings)))
