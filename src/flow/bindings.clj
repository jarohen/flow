(ns flow.bindings)

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
