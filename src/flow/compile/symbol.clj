(ns flow.compile.symbol
  (:require [flow.compile :refer [compile-form compile-value]]
            [flow.protocols :as fp]))

(defmethod compile-form :symbol [{:keys [sym]} {:keys [dynamic-syms local-syms]}]
  (let [dynamic? (contains? dynamic-syms sym)
        local? (contains? local-syms sym)]
    (reify fp/CompiledForm
      (form-deps [_] (when dynamic?
                       #{sym}))

      (bindings [_] nil)

      (initial-value-form [_ state-sym]
        (if (or dynamic? local?)
          `(get ~state-sym (quote ~sym))
          sym))

      (updated-value-form [_ new-state-sym updated-vars-sym]
        (if (or dynamic? local?)
          `(get ~new-state-sym (quote ~sym))
          sym)))))

(defmethod compile-value :symbol [{:keys [sym]} {:keys [dynamic-syms local-syms]}]
  (let [dynamic? (contains? dynamic-syms sym)
        local? (contains? local-syms sym)]
    (reify fp/CompiledValue
      (value-deps [_] (when dynamic?
                        #{sym}))

      (inline-value [_ state-sym]
        (if (or dynamic? local?)
          `(get ~state-sym (quote ~sym))
          sym)))))
