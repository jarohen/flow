(ns flow.compile.simple
  (:require [flow.compile :refer [compile-el]]))

(defmethod compile-el :map [m opts]
  {:as-value (->> (for [[k v] m]
                    [(:as-value (compile-el k opts))
                     (:as-value (compile-el v opts))])
              
                  (into {}))})

(defmethod compile-el :coll [coll opts]
  {:as-value (map (comp :as-value #(compile-el % opts)) coll)})

(defmethod compile-el :symbol [{:keys [symbol]} {:keys [dynamic-syms]}]
  {:as-value (or (get-in dynamic-syms [symbol :unshadowed-sym])
                 symbol)})

(defmethod compile-el :primitive [{:keys [primitive elem?]} opts]
  (if (nil? primitive)
    nil
    
    {:el-init `(js/document.createTextNode ~(str primitive))
     :as-value primitive}))
