(ns flow.protocols)

(defprotocol CompiledIdentity
  (identity-deps [_])
  (bindings [_])
  (initial-form [_ state-sym])
  (updated-form [_ new-state-sym updated-vars-sym]))

(extend-protocol CompiledIdentity
  nil
  (identity-deps [_] nil)
  (bindings [_] nil)
  (initial-form [_ state-sym] nil)
  (updated-form [_ new-state-sym updated-vars-sym] nil))

(defprotocol CompiledValue
  (value-deps [_])
  (inline-value-form [_ state-sym]))

(extend-protocol CompiledValue
  nil
  (value-deps [_] nil)
  (inline-value-form [_ state-sym] nil))
