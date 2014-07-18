(ns flow.compile.symbol
  (:require [flow.compile :refer [compile-form]]
            [flow.protocols :as fp]))

(defmethod compile-form :symbol [{:keys [sym]} {:keys [dynamic-syms local-syms state]}]
  (let [dynamic? (contains? dynamic-syms sym)
        local? (contains? local-syms sym)]
    (reify fp/CompiledForm
      (form-deps [_] (when dynamic?
                       #{sym}))
      (form-declarations [_] nil)

      (bindings [_] nil)

      (current-value-form [_ state-sym]
        `(get ~state-sym (quote ~sym))))))
