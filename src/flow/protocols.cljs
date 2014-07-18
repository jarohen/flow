(ns flow.protocols
  (:require [flow.dom :as fd]))

(defprotocol Box
  (should-update? [_ updated-vars])
  (build [_ state])
  (handle-update! [_ old-state new-state updated-vars]))

(extend-protocol Box
  nil
  (should-update? [_ updated-vars]
    false)

  (build [_ state]
    (fd/null-elem))

  (handle-update! [_ old-state new-state updated-vars]
    nil)


  js/Text
  (should-update? [_ updated-vars]
    false)

  (build [text-node state]
    text-node)

  (handle-update! [_ old-state new-state updated-vars]
    nil))
