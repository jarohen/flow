(ns flow.protocols)

(defprotocol FlowElement
  (get-flow-id [_])
  (set-flow-id! [_ id]))

(extend-protocol FlowElement
  object

  (get-flow-id [$el]
    (or (.-flowId $el) $el))

  (set-flow-id! [$el id]
    (set! (.-flowId $el) id)))

