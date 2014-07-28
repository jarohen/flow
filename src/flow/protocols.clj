(ns flow.protocols)

(defprotocol CompiledForm
  (form-deps [_])
  (bindings [_ state-sym])
  (initial-value-form [_ state-sym])
  (updated-value-form [_ new-state-sym updated-vars-sym]))

(extend-protocol CompiledForm
  nil
  (form-deps [_] nil)
  (bindings [_ state-sym] nil)
  (initial-value-form [_ state-sym] nil)
  (updated-value-form [_ new-state-sym updated-vars-sym] nil))

(defprotocol CompiledValue
  (value-deps [_])
  (inline-value [_ state-sym]))

(extend-protocol CompiledForm
  nil
  (value-deps [_] nil)
  (inline-value [_ state-sym] nil))
