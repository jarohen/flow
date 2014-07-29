(ns flow.compile.simple
  (:require [flow.compile :refer [compile-el compile-value]]
            [flow.protocols :as fp]))

(alias 'fd (doto 'flow.dom create-ns))

;; PRIMITIVE

(defmethod compile-value :primitive [{:keys [primitive]} opts]
  (reify fp/CompiledForm
    (form-deps [_] nil)

    (bindings [_])
    (initial-value-form [_ _] primitive)
    (updated-value-form [_ _ _] primitive)))

(defmethod compile-el :primitive [form opts]
  (compile-value form opts))

;; MAP

#_(defmethod compile-el :map [{m :map} opts]
    (let [flattened-map (flatten (seq m))
          compiled-elems (map #(compile-el % opts) flattened-map)]
      {:deps (set (mapcat :deps compiled-elems))
       :inline-value (->> compiled-elems
                          (map :inline-value)
                          (partition 2)
                          (into {}))}))

;; COLL

#_(defmethod compile-el :coll [{:keys [coll]} opts]
    (let [compiled-elems (map #(compile-el % opts) coll)]
      {:inline-value `(into ~(empty coll) [~@(map :inline-value compiled-elems)])
       :deps (set (mapcat :deps compiled-elems))}))
