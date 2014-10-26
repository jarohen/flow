(ns flow.forms.fn-calls
  (:require #+clj [flow.compiler :as fc]))

(defn build-fn-call [call-fn]
  (fn []
    (letfn [(update-call! []
              [(call-fn) update-call!])]
      
      (update-call!))))

#+clj
(defmethod fc/compile-el-form :fn-call [call-args opts]
  `(build-fn-call (fn []
                    ~(map #(fc/compile-value-form % opts) call-args))))

#+clj
(defmethod fc/compile-value-form :fn-call [call-args opts]
  (map #(fc/compile-value-form % opts) call-args))
