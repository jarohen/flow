(ns flow.bindings
  (:require [flow.compile :refer [compile-identity compile-value]]
            [flow.util :as u]
            [flow.protocols :as fp]
            [clojure.set :as set]))

(alias 'fs (doto 'flow.state create-ns))

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

(defn compile-identity-binding [{:keys [bind value idx]} opts]
  (let [compiled-identity (compile-identity value (u/with-more-path opts [(str idx)]))
        
        hard-deps (fp/hard-deps compiled-identity)
        soft-deps (fp/soft-deps compiled-identity)

        destructured-syms (destructuring-bind-syms bind)]

    {:compiled-binding {:hard-deps hard-deps
                        :soft-deps soft-deps
                        :deps (set/union hard-deps soft-deps)

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

(defn compile-identity-bindings [bindings opts]
  (reduce (fn [{:keys [compiled-bindings opts] :as acc} binding]
            (let [{:keys [compiled-binding opts]} (compile-identity-binding binding opts)]

              {:compiled-bindings (conj compiled-bindings
                                        compiled-binding)
               
               :opts opts}))
                
          {:compiled-bindings []
           :opts opts}
                
          bindings))

(defn identity-bindings-deps [compiled-bindings compiled-body]
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

(defn compile-value-binding [{:keys [bind value idx]} {:keys [state-sym] :as opts}]
  (let [compiled-value (compile-value value opts)
        
        deps (fp/value-deps compiled-value)

        destructured-syms (destructuring-bind-syms bind)]

    {:compiled-binding {:deps deps
                        :bound-syms destructured-syms

                        :value-bindings `[[~bind (binding [fs/*state* ~state-sym]
                                                   ~(fp/inline-value-form compiled-value))]]
                        :state-bindings `[[~state-sym (merge ~state-sym
                                                             ~(->> (for [bind-sym destructured-syms]
                                                                     `[(quote ~bind-sym) ~bind-sym])
                                                                   (into {})))]]}

     :opts (update-in opts
                      [(if (seq deps)
                         :dynamic-syms
                         :local-syms)]
                      set/union destructured-syms)}))

(defn compile-value-bindings [bindings opts]
  (reduce (fn [{:keys [compiled-bindings opts] :as acc} binding]
            (let [{:keys [compiled-binding opts]} (compile-value-binding binding opts)]

              {:compiled-bindings (conj compiled-bindings
                                        compiled-binding)
               
               :opts opts}))
                
          {:compiled-bindings []
           :opts opts}
                
          bindings))

(defn value-bindings-deps [compiled-bindings compiled-body]
  (reduce (fn [deps {:keys [bound-syms] :as compiled-binding}]
            (-> deps
                (set/difference (:bound-syms compiled-binding))
                (set/union (when (seq (set/intersection bound-syms deps))
                             (:deps compiled-binding)))))

          (fp/value-deps compiled-body)
          
          (reverse compiled-bindings)))
