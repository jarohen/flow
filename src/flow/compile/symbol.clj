(ns flow.compile.symbol
  (:require [flow.compile :refer [compile-el compile-value]]
            [flow.protocols :as fp]))

(defmethod compile-el :symbol [{:keys [sym]} {:keys [dynamic-syms local-syms]}]
  (let [dynamic? (contains? dynamic-syms sym)
        local? (contains? local-syms sym)]
    (reify fp/CompiledElement
      (elem-deps [_] (when dynamic?
                       #{sym}))

      (bindings [_] nil)

      (initial-el-form [_ state-sym]
        (if (or dynamic? local?)
          `(get ~state-sym (quote ~sym))
          sym))

      (updated-el-form [_ new-state-sym updated-vars-sym]
        (if (or dynamic? local?)
          `(get ~new-state-sym (quote ~sym))
          sym)))))

(defmethod compile-value :symbol [{:keys [sym]} {:keys [dynamic-syms local-syms]}]
  (let [dynamic? (contains? dynamic-syms sym)
        local? (contains? local-syms sym)]
    (reify fp/CompiledValue
      (value-deps [_] (when dynamic?
                        #{sym}))

      (inline-value-form [_ state-sym]
        (if (or dynamic? local?)
          `(get ~state-sym (quote ~sym))
          sym)))))
