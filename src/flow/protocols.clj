(ns flow.protocols)

(defprotocol CompiledForm
  (form-deps [_])
  (bindings [_])
  (initial-value-form [_ state-sym])
  (updated-value-form [_ new-state-sym updated-vars-sym]))

(extend-protocol CompiledForm
  nil
  (form-deps [_] nil)
  (bindings [_] nil)
  (initial-value-form [_ state-sym] nil)
  (updated-value-form [_ new-state-sym updated-vars-sym] nil))
