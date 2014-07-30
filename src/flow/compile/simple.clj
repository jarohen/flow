(ns flow.compile.simple
  (:require [flow.compile :refer [compile-el compile-value]]
            [flow.protocols :as fp]))

(alias 'fd (doto 'flow.dom create-ns))

;; PRIMITIVE

(defmethod compile-value :primitive [{:keys [primitive]} opts]
  (reify fp/CompiledValue
    (value-deps [_] nil)
    (inline-value-form [_ _] primitive)))

(defmethod compile-el :primitive [{:keys [primitive]} opts]
  (reify fp/CompiledElement
    (elem-deps [_] nil)

    (bindings [_])
    (initial-el-form [_ _] primitive)
    (updated-el-form [_ _ _] primitive)))

;; MAP

(defmethod compile-value :map [{m :map} opts]
  (let [flattened-map (flatten (seq m))
        compiled-values (map #(compile-value % opts) flattened-map)]
    (reify fp/CompiledValue
      (value-deps [_]
        (set (mapcat fp/value-deps compiled-values)))

      (inline-value-form [_ state-sym]
        (->> compiled-values
             (map #(fp/inline-value-form % state-sym))
             (partition 2)
             (map #(apply vector %))
             (into {}))))))

(defmethod compile-el :map [form opts]
  (let [compiled-map (compile-value form opts)]
    (reify fp/CompiledElement
      (elem-deps [_] (fp/value-deps compiled-map))
      (bindings [_] nil)

      (initial-el-form [_ state-sym]
        (fp/inline-value-form compiled-map state-sym))

      (updated-el-form [_ new-state-sym updated-vars-sym]
        (fp/inline-value-form compiled-map new-state-sym)))))

;; COLL

(defmethod compile-value :coll [{:keys [coll]} opts]
  (let [compiled-values (map #(compile-value % opts) coll)]
    (reify fp/CompiledValue
      (value-deps [_]
        (set (mapcat fp/value-deps compiled-values)))

      (inline-value-form [_ state-sym]
        `(into ~(empty coll) [~@(map #(fp/inline-value-form % state-sym) compiled-values)])))))

(defmethod compile-el :coll [form opts]
  (let [compiled-coll (compile-value form opts)]
    (reify fp/CompiledElement
      (elem-deps [_] (fp/value-deps compiled-coll))
      (bindings [_] nil)

      (initial-el-form [_ state-sym]
        (fp/inline-value-form compiled-coll state-sym))

      (updated-el-form [_ new-state-sym updated-vars-sym]
        (fp/inline-value-form compiled-coll new-state-sym)))))
