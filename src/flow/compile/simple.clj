(ns flow.compile.simple
  (:require [flow.compile :refer [compile-el compile-value-form]]))

(alias 'fd (doto 'flow.dom create-ns))
(alias 'fp (doto 'flow.protocols create-ns))

(defmethod compile-value-form :map [{m :map} opts]
  (let [flattened-map (flatten (seq m))
        compiled-elems (map #(compile-value-form % opts))]
    {:deps (set (mapcat :deps compiled-elems))
     :inline-value (->> compiled-elems
                        (map :inline-value)
                        (partition 2)
                        (into {}))}))

(defmethod compile-value-form :coll [{:keys [coll]} opts]
  (let [compiled-elems (map #(compile-value-form % opts) coll)]
    {:inline-value `(into ~(empty coll) [~@(map :inline-value compiled-elems)])
     :deps (set (mapcat :deps compiled-elems))}))

(defmethod compile-value-form :symbol [{:keys [symbol]} {:keys [dynamic-syms local-syms state-sym]}]
  (let [dynamic? (contains? dynamic-syms symbol)
        local? (contains? local-syms symbol)]
    {:deps (when dynamic?
             #{symbol})
     :inline-value (if (or dynamic? local?)
                     `(get ~state-sym (quote ~symbol))
                     symbol)}))

(defmethod compile-el :primitive [{:keys [primitive elem?]} opts]
  {:el-return (if (nil? primitive)
                `(fd/null-elem)
                `(js/document.createTextNode ~(str primitive)))})

(defmethod compile-value-form :primitive [{:keys [primitive elem?]} opts]
  {:inline-value primitive})
