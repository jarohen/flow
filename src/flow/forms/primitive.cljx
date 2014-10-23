(ns flow.forms.primitive
  (:require #+clj [flow.compiler :as fc]
            [flow.dom.attributes :as fda]
            [flow.dom.children :as fdc]
            [flow.dom.elements :as fde]))

(defn primitive-value [primitive]
  (fn update! []
    [primitive update!]))

#+clj
(defmethod fc/compile-value-form :primitive [primitive]
  `(primitive-value ~primitive))
