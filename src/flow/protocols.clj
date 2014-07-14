(ns flow.protocols)

(defprotocol DynamicValueForm
  (initial-value-form [_ value-sym state-sym])
  (updated-value-form [_ value-sym old-state-sym new-state-sym updated-vars-sym]))
