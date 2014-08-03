(ns flow.compile.symbol
  (:require [flow.compile :refer [compile-identity compile-value]]
            [flow.protocols :as fp]))

(alias 'fs (doto 'flow.state create-ns))

(defmethod compile-value :symbol [{:keys [sym]} {:keys [dynamic-syms local-syms]}]
  (let [dynamic? (contains? dynamic-syms sym)
        local? (contains? local-syms sym)]
    (reify fp/CompiledValue
      (value-deps [_]
        (when dynamic?
          #{sym}))

      (inline-value-form [_]
        (if (or dynamic? local?)
          `(get fs/*state* (quote ~sym))
          sym)))))
