(ns flow.compile.simple
  (:require [flow.compile :refer [compile-el compile-value-form]]
            [flow.util :as u]))

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

(defmethod compile-el :symbol [{:keys [symbol path]} {:keys [dynamic-syms local-syms state-sym updated-vars]}]
  (let [dynamic? (contains? dynamic-syms symbol)
        el (symbol path)
        deps (when dynamic?
               #{symbol})]
    {:deps deps
     :el `(~el)
     :declarations [`(defn ~el []
                       (let [!placeholder-el# (atom nil)]
                         (reify fp/DynamicElement
                           (~'should-update-el? [_# ~updated-vars]
                             ~(u/deps->should-update deps updated-vars))

                           (~'build-element [_# state#]
                             (let [initial-el# (fd/->node (get state# (quote ~symbol)))]
                               (reset! !placeholder-el# initial-el#)
                               initial-el#))

                           (~'handle-update! [_# old-state# new-state# updated-vars#]
                             (let [new-el# (fd/->node (get new-state# (quote ~symbol)))]
                               (fd/swap-elem! @!placeholder-el# new-el#)
                               (reset! !placeholder-el# new-el#))))))]

     :el-return (when-not dynamic?
                  `(fd/->node (get ~state-sym (quote ~symbol))))}))

(defmethod compile-el :primitive [{:keys [primitive elem?]} opts]
  {:el-return `(fd/->node primitive)})

(defmethod compile-value-form :primitive [{:keys [primitive elem?]} opts]
  {:inline-value primitive})
