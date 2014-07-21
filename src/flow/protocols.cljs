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
