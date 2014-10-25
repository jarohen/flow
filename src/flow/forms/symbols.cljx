(ns flow.forms.symbols
  (:require #+clj [flow.compiler :as fc]))

;; TODO - as and when we have let bindings - this is going to need to
;; check whether it's a dynamic symbol or not.

(defn build-symbol [symbol-fn]
  (fn []
    (letfn [(update-symbol! []
              [(symbol-fn) update-symbol!])]
      (update-symbol!))))

#+clj
(defmethod fc/compile-el-form :symbol [symbol opts]
  `(build-symbol (fn [] ~symbol)))

#+clj
(defmethod fc/compile-value-form :symbol [symbol opts]
  symbol)
