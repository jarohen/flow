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
