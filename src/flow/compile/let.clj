(ns flow.compile.let
  (:require [flow.compile :refer [compile-form]]
            [flow.compile.calls :refer [compile-call-form]]
            [flow.bindings :as b]
            [flow.util :as u]
            [clojure.set :as set]
            [flow.protocols :as fp]))

#_(defmethod compile-call-form :let [{:keys [bindings body path]} {:keys [state] :as opts}]
    (let [{:keys [compiled-bindings opts]} (b/compile-bindings bindings opts)

          compiled-body (compile-form body opts)

          deps (b/bindings-deps compiled-bindings compiled-body)

          let-sym (symbol path)]

      {:el `(~let-sym)
       :deps deps
       :declarations (concat (mapcat :declarations (concat compiled-bindings [compiled-body]))
                             [`(defn ~let-sym []
                                 (let [~@(mapcat (juxt :bind-values->map-sym :bind-values->map) compiled-bindings)]
                                   (flow.forms.let/let->el ~(u/quote-deps deps)

                                                           ~(:el compiled-body)

                                                           (fn let-bindings-state# [~state]
                                                             (let [~@(mapcat (fn [{:keys [inline-value value-sym bind-values->map-sym]}]
                                                                               `[~value-sym ~inline-value
                                                                                 ~state (merge ~state (~bind-values->map-sym ~value-sym))])

                                                                             compiled-bindings)]
                                                               ~state)))))])}))

