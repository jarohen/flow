(ns flow.compile.simple
  (:require [flow.compile :refer [compile-form compile-value]]
            [flow.protocols :as fp]))

(alias 'fd (doto 'flow.dom create-ns))

;; PRIMITIVE

(defmethod compile-form :primitive [{:keys [primitive]} opts]
  (reify fp/CompiledForm
    (form-deps [_] nil)

    (bindings [_])
    (initial-value-form [_ _] primitive)
    (updated-value-form [_ _ _] primitive)))

(defmethod compile-value :primitive [{:keys [primitive]} opts]
  (reify fp/CompiledValue
    (value-deps [_] nil)
    (inline-value [_ state-sym] primitive)))

;; MAP

#_(defmethod compile-form :map [{m :map} opts]
    (let [flattened-map (flatten (seq m))
          compiled-elems (map #(compile-form % opts) flattened-map)]
      {:deps (set (mapcat :deps compiled-elems))
       :inline-value (->> compiled-elems
                          (map :inline-value)
                          (partition 2)
                          (into {}))}))

;; COLL

#_(defmethod compile-form :coll [{:keys [coll]} opts]
    (let [compiled-elems (map #(compile-form % opts) coll)]
      {:inline-value `(into ~(empty coll) [~@(map :inline-value compiled-elems)])
       :deps (set (mapcat :deps compiled-elems))}))
