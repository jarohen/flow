(ns flow.compile.let
  (:require [flow.compile.calls :refer [compile-call-form]]
            [flow.compile :refer [compile-form]]
            [flow.bindings :as b]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-form :let [{:keys [bindings body path]} {:keys [state] :as opts}]
  (let [{:keys [compiled-bindings opts]} (b/compile-bindings bindings opts)

        compiled-body (compile-form body opts)

        deps (b/bindings-deps compiled-bindings compiled-body)

        let-sym (symbol path)]

    (reify fp/CompiledForm
      (form-deps [_] deps)

      (bindings [_]
        (concat (mapcat fp/bindings (concat compiled-bindings [compiled-body]))))

      (initial-value-form [_ state-sym]
        (let [~@(mapcat (fn [{:keys [value-sym bind-values->map-sym compiled-value]}]
                          `[~value-sym ~(fp/initial-value-form compiled-value ~state-sym)
                            ~state-sym (merge ~state-sym (~bind-values->map-sym ~value-sym))])

                        compiled-bindings)]
          ~state)))
    
    {:declarations (concat (mapcat :declarations (concat compiled-bindings [compiled-body]))
                           [`(defn ~let-sym []
                               (let [~@(mapcat (juxt :bind-values->map-sym :bind-values->map) compiled-bindings)]
                                 (flow.forms.let/let->el ~(u/quote-deps deps)

                                                         ~(:el compiled-body)

                                                         (fn let-bindings-state# [~state]
                                                           ))))])}))

