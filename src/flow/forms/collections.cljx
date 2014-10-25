(ns flow.forms.collections
  (:require #+clj [flow.compiler :as fc]))

(defmethod fc/compile-value-form :coll [form opts]
  (->> form
       (map #(fc/compile-value-form % opts))
       (into (empty form))))

