(ns flow.compile.simple
  (:require [flow.compile :refer [compile-el]]))

(defmethod compile-el :map [m opts]
  {:init (->> (for [[k v] m]
                [(:init (compile-el k opts))
                 (:init (compile-el v opts))])
              
              (into {}))})

(defmethod compile-el :coll [coll opts]
  {:init (map (comp :init #(compile-el % opts)) coll)})

(defmethod compile-el :primitive [{:keys [primitive elem?]} opts]
  (if (nil? primitive)
    nil
    {:init (if elem?
             `(js/document.createTextNode ~(str primitive))
             primitive)}))
