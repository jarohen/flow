(ns flow.compile.for
  (:require [flow.compile :refer [compile-form]]
            [flow.compile.calls :refer [compile-call-form]]
            [flow.bindings :as b]
            [flow.util :as u]
            [clojure.set :as set]
            [flow.protocols :as fp]))

(alias 'f (doto 'flow.core create-ns))

#_(defmethod compile-call-form :for [{:keys [bindings body path]} {:keys [state] :as opts}]
    (let [{:keys [compiled-bindings opts]} (b/compile-bindings bindings opts)

          compiled-body (compile-form body opts)

          deps (b/bindings-deps compiled-bindings compiled-body)
        
          for-sym (symbol path)]

      {:el `(~for-sym)
       :deps deps
       :declarations (concat (:declarations compiled-body)
                           
                             [`(defn ~for-sym []
                                 (let [~@(mapcat (juxt :bind-values->map-sym :bind-values->map) compiled-bindings)]
                                   
                                   (flow.forms.for/for->el ~(u/quote-deps deps)

                                                           (fn for-values# [~state]
                                                             (for [~@(mapcat (fn [{:keys [inline-value value-sym bind-values->map-sym]}]
                                                                               `[~value-sym ~inline-value
                                                                                 :let [~state (merge ~state (~bind-values->map-sym ~value-sym))]])

                                                                             compiled-bindings)]

                                                               {:keys (map (fn [key-fn# value#]
                                                                             (or (when key-fn#
                                                                                   (key-fn# value#))
                                                                                 (::f/id value#)
                                                                                 (:id value#)
                                                                                 value#))
                                                                           
                                                                           [~@(map :key-fn bindings)]
                                                                           [~@(map :value-sym compiled-bindings)])
                                                                :state ~state}))
                                                           
                                                           (fn []
                                                             ~(:el compiled-body)))))])}))

