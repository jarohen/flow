(ns flow.compile.wrap-cursor
  (:require [flow.compile.calls :refer [compile-call-el compile-call-value]]
            [flow.protocols :as fp]))

(defmethod compile-call-value :wrap-cursor [{:keys [cursor]} opts]
  )

