(ns flow.protocols
  (:require [flow.dom :as fd]))

(defprotocol DynamicValue
  (should-update-value? [_ updated-vars])
  (current-value [_ state]))

(defprotocol DynamicElement
  (should-update-el? [_ updated-vars])
  (build-element [_ state])
  (handle-update! [_ old-state new-state updated-vars]))

(extend-protocol DynamicValue
  nil
  (should-update-value? [_ updated-vars]
    false)

  (current-value [_ state]
    nil))

(extend-protocol DynamicElement
  nil
  (should-update-el? [_ updated-vars]
    false)

  (build-element [_ state]
    (fd/null-elem))

  (handle-update! [_ old-state new-state updated-vars]
    nil))
