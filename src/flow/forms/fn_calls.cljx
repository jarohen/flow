(ns flow.forms.fn-calls
  (:require #+clj [flow.compiler :as fc]))

#+clj
(defmethod fc/compile-value-form :fn-call [call-args opts]
  (map #(fc/compile-value-form % opts) call-args))
