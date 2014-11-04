(ns flow.forms.primitive
  (:require #+clj [flow.compiler :as fc]))

(defn build-primitive [primitive]
  (fn update-primitive! []
    [primitive update-primitive!]))

#+clj
(defmethod fc/compile-el-form :primitive [primitive opts]
  `(build-primitive ~primitive))

#+clj
(defmethod fc/compile-value-form :primitive [primitive opts]
  primitive)
