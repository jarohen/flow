(ns flow.compile)

(defmulti compile-el
  (fn [elem env]
    (:type elem)))

(require 'flow.compile.node)

(defmethod compile-el :primitive [{:keys [primitive elem?]} opts]
  (if (nil? primitive)
    nil
    {:init (if elem?
             `(js/document.createTextNode ~(str primitive))
             primitive)}))
