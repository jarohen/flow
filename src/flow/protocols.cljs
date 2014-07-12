(ns flow.protocols
  (:require [flow.dom :as fd]))

(defprotocol DynamicValue
  (current-value [_ state]))

(defprotocol DynamicElement
  (should-update? [_ updated-vars])
  (build-element [_ state])
  (handle-update! [_ old-state new-state updated-vars]))

(extend-protocol DynamicValue
  nil
  (current-value [_ state]
    nil))

(extend-protocol DynamicElement
  nil
  (should-update? [_ updated-vars]
    false)

  (build-element [_ state]
    (fd/null-elem))

  (handle-update! [_ old-state new-state updated-vars]
    nil))
