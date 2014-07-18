(ns flow.compile.simple
  (:require [flow.compile :refer [compile-form]]
            [flow.util :as u]
            [flow.protocols :as fp]))

(alias 'fd (doto 'flow.dom create-ns))

(defn value->el [{:keys [deps inline-value]}]
  )

;; PRIMITIVE

(defmethod compile-form :primitive [{:keys [primitive elem?]} opts]
  (reify fp/CompiledForm
    (form-deps [_] nil)
    (form-declarations [_] nil)

    (bindings [_] nil)

    (current-value-form [_ state-sym]
      primitive)))

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


