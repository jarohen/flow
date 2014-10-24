(ns flow.forms.primitive
  (:require #+clj [flow.compiler :as fc]))

#+clj
(defmethod fc/compile-value-form :primitive [primitive opts]
  primitive)
