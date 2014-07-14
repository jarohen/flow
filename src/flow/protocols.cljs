(ns flow.protocols
  (:require [flow.dom :as fd]))

(defprotocol DynamicElement
  (should-update? [_ updated-vars])
  (build-element [_ state])
  (handle-update! [_ old-state new-state updated-vars]))

(extend-protocol DynamicElement
  nil
  (should-update? [_ updated-vars]
    false)

  (build-element [_ state]
    (fd/null-elem))

  (handle-update! [_ old-state new-state updated-vars]
    nil)


  js/Text
  (should-update? [_ updated-vars]
    false)

  (build-element [text-node state]
    text-node)

  (handle-update! [_ old-state new-state updated-vars]
    nil))

(defprotocol DynamicValue
  (should-update-value? [_ updated-vars])
  (initial-value [_ state])
  (updated-value [_ old-state new-state updated-vars]))

(extend-protocol DynamicValue
  nil
  (should-update-value? [_ updated-vars]
    false)
  (initial-value [_ state]
    nil)
  (updated-value [_ old-state new-state updated-vars]
    nil))
