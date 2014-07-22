(ns flow.compile.for
  (:require [flow.compile.calls :refer [compile-call-form]]
            [flow.compile :refer [compile-form]]
            [flow.bindings :as b]
            [flow.util :as u]))

(alias 'f (doto 'flow.core create-ns))

(defmethod compile-call-form :for [{:keys [bindings body path]} opts]
  (let [{:keys [compiled-bindings opts]} (b/compile-bindings bindings opts)

        compiled-body (compile-form body opts)

        deps (b/bindings-deps compiled-bindings compiled-body)

        for-sym (symbol path)]

    (reify fp/CompiledForm
      (form-deps [_] deps)

      (bindings [_]
        (concat (mapcat bp/bindings compiled-bindings)
                (fp/bindings compiled-body)

                (let [state (symbol (str path "-state"))
                      new-state (symbol (str path "-new-state"))
                      updated-vars (symbol (str path "-updated-vars"))]
                  
                  `[[~for-sym (let [!body-value-cache# (atom {})
                                    make-body# (fn []
                                                 (reify fp/DynamicValue
                                                   (~'build [~'_ ~state]
                                                     ~(fp/initial-value-form compiled-body state))
                                                   (~'updated-value [~'_ ~new-state ~updated-vars]
                                                     ~(fp/updated-value-form compiled-body new-state updated-vars))))]
                                
                                (reify fp/DynamicValue
                                  (~'build [~'_ ~state]
                                    (for [~@(->> (for [[value-bindings state-bindings] (->> (mapcat #(bp/initial-bindings % state) compiled-bindings))]
                                                   [(apply concat value-bindings)
                                                    :let (apply concat state-bindings)])
                                                 (apply concat))]
                                                                        
                                      ))

                                  (~'updated-value [~'_ ~new-state ~updated-vars]
                                    (for [~@(->> (for [[value-bindings state-bindings] (->> (mapcat #(bp/updated-bindings % new-state updated-vars) compiled-bindings))]
                                                   [(apply concat value-bindings)
                                                    :let (apply concat state-bindings)])
                                                 (apply concat))]
                                      (let [keys# [~@(map bp/value-sym compiled-bindings)]]
                                        ~(fp/updated-value-form compiled-body new-state updated-vars))))))]])))

      (initial-value-form [_ state-sym]
        `(fp/build ~for-sym ~state-sym))

      (updated-value-form [_ new-state-sym updated-vars-sym]
        `(fp/updated-value ~for-sym ~new-state-sym ~updated-vars-sym)))))

