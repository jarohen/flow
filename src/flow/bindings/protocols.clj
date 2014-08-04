(ns flow.bindings.protocols)

(defprotocol CompiledBindings
  (destructured-syms [_])
  (value-deps [_])
  (value-key [_])
  (bindings [_])
  (initial-bindings [_ state-sym])
  (updated-bindings [_ new-state-sym updated-vars-sym]))

