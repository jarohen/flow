(ns flow.compile.simple
  (:require [flow.compile :refer [compile-identity compile-value]]
            [flow.protocols :as fp]))

(alias 'fd (doto 'flow.dom create-ns))
(alias 'fs (doto 'flow.state create-ns))

;; PRIMITIVE

(defmethod compile-value :primitive [{:keys [primitive]} opts]
  (reify fp/CompiledValue
    (value-deps [_] nil)
    (inline-value-form [_] primitive)))

;; MAP

(defmethod compile-value :map [{m :map} opts]
  (let [flattened-map (flatten (seq m))
        compiled-values (map #(compile-value % opts) flattened-map)]
    (reify fp/CompiledValue
      (value-deps [_]
        (set (mapcat fp/value-deps compiled-values)))

      (inline-value-form [_]
        (->> compiled-values
             (map fp/inline-value-form)
             (partition 2)
             (map #(apply vector %))
             (into {}))))))

;; COLL

(defmethod compile-value :coll [{:keys [coll]} opts]
  (let [compiled-values (map #(compile-value % opts) coll)]
    (reify fp/CompiledValue
      (value-deps [_]
        (set (mapcat fp/value-deps compiled-values)))

      (inline-value-form [_]
        `(into ~(empty coll) [~@(map fp/inline-value-form compiled-values)])))))
