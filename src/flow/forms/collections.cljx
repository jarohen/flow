(ns flow.forms.collections
  (:require #+clj [flow.compiler :as fc]))

#+clj
(defmethod fc/compile-value-form :coll [coll opts]
  (->> coll
       (map #(fc/compile-value-form % opts))
       (into (empty coll))))

#+clj
(defmethod fc/compile-value-form :map [m opts]
  (->> (for [[k v] m]
         [(fc/compile-value-form k opts) (fc/compile-value-form v opts)])
       (into {})))
