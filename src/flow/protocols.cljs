(ns flow.protocols
  (:require [flow.dom :as fd]))

(defprotocol DynamicValue
  (build [_ state])
  (updated-value [_ new-state updated-vars]))

(extend-protocol DynamicValue
  nil
  (build [_ _]
    nil)

  (updated-value [_ _ _]
    nil))


(defprotocol FlowElement
  (get-flow-id [_])
  (set-flow-id! [_ id]))

(extend-protocol FlowElement
  js/Node

  (get-flow-id [$el]
    (or (.-flowId $el) $el))

  (set-flow-id! [$el id]
    (set! (.-flowId $el) id)))

