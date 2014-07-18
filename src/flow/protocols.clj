(ns flow.protocols)

(defprotocol CompiledForm
  (form-deps [_])
  (form-declarations [_])
  
  (bindings [_])

  (should-update-form [_ updated-vars-sym])
  (handle-update-form [_ old-state-sym new-state-sym updated-vars-sym])
  
  (current-value-form [_ state-sym]))
