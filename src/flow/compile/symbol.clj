(ns flow.compile.symbol
  (:require [flow.compile :refer [compile-form]]
            [flow.protocols :as fp]))

(defmethod compile-form :symbol [{:keys [sym]} {:keys [dynamic-syms]}]
  (let [dynamic? (contains? dynamic-syms sym)]
    (reify fp/CompiledForm
      (form-deps [_] (when dynamic?
                       #{sym}))

      (bindings [_] nil)

      (initial-value-form [_ state-sym]
        (if dynamic?
          `(get ~state-sym (quote ~sym))
          sym))

      (updated-value-form [_ old-state-sym new-state-sym updated-vars-sym]
        (if dynamic?
          `(get ~new-state-sym (quote ~sym))
          sym)))))
