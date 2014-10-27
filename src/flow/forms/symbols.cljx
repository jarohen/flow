(ns flow.forms.symbols
  (:require #+clj [flow.compiler :as fc]
            [flow.state :as fs]))

(defn build-symbol [sym-fn]
  (fn []
    (letfn [(update-symbol! []
              [(sym-fn) update-symbol!])]
      (update-symbol!))))

#+clj
(defn symbol-form [sym bound-syms]
  (if (contains? bound-syms sym)
    `(get fs/*state* (quote ~sym))
    sym))

#+clj
(defmethod fc/compile-el-form :symbol [sym {:keys [bound-syms] :as opts}]
  `(build-symbol (fn [] ~(symbol-form sym bound-syms))))

#+clj
(defmethod fc/compile-value-form :symbol [sym {:keys [bound-syms] :as opts}]
  (symbol-form sym bound-syms))
