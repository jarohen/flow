(ns flow.bindings
  (:require [flow.compile :refer [compile-form]]
            [clojure.set :as set]))

(defn init-bindings [bindings]
  (->> (for [[bind value] bindings]
         `[(quote ~bind) ~value])
      
       (into {})))

(defn read-bindings [!bindings-sym bindings]
  (mapcat (fn [[bind _]]
            [bind `(get @~!bindings-sym (quote ~bind))])
          
          bindings))

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

(defn with-bind-values->map-fn [{:keys [bind destructured-binds] :as compiled-el}]
  (assoc compiled-el
    :bind-values->map `(fn bind-values->map# [value#]
                         (let [~bind value#]
                           ~(->> (for [bind-sym destructured-binds]
                                   `[(quote ~bind-sym) ~bind-sym])
                                 (into {}))))))

(defn compile-bindings [bindings {:keys [dynamic-syms local-syms] :as opts}]
  (reduce (fn [{:keys [compiled-bindings opts] :as acc} {:keys [bind value path]}]
            (let [{:keys [dynamic-syms local-syms]} opts

                  {:keys [deps] :as compiled-value}
                  (compile-form value (assoc opts
                                         :dynamic-syms dynamic-syms
                                         :local-syms local-syms))
                        
                  destructured-binds (destructuring-bind-syms bind)]
                    
              (-> acc
                  (update-in [:opts (if (empty? deps) :local-syms :dynamic-syms)]
                             set/union destructured-binds)
                  (update-in [:compiled-bindings]
                             conj (-> compiled-value
                                      (assoc :bind bind
                                             :destructured-binds destructured-binds
                                             :value-sym (symbol (str path "-value"))
                                             :bind-values->map-sym (symbol (str path "-value->map")))
                                      with-bind-values->map-fn)))))
                
          {:compiled-bindings []
           :opts opts}
                
          bindings))

(defn bindings-values [compiled-bindings {:keys [state]}]
  )

(defn bindings-deps [compiled-bindings compiled-body]
  (reduce (fn [deps-acc {:keys [deps destructured-binds]}]
            (-> deps-acc
                (set/difference destructured-binds)
                (set/union deps)))
          (:deps compiled-body)
          (reverse compiled-bindings)))
