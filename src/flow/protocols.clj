(ns flow.protocols)

(defprotocol CompiledElement
  (elem-deps [_])
  (bindings [_])
  (initial-el-form [_ state-sym])
  (updated-el-form [_ new-state-sym updated-vars-sym]))

(extend-protocol CompiledElement
  nil
  (elem-deps [_] nil)
  (bindings [_] nil)
  (initial-el-form [_ state-sym] nil)
  (updated-el-form [_ new-state-sym updated-vars-sym] nil))

(defprotocol CompiledValue
  (value-deps [_])
  (inline-value-form [_ state-sym]))

(extend-protocol CompiledValue
  nil
  (value-deps [_] nil)
  (inline-value-form [_ state-sym] nil))
