(ns flow.compile.let
  (:require [flow.compile.calls :refer [compile-call-form]]
            [flow.compile :refer [compile-form]]
            [flow.bindings :as b]
            [flow.bindings.protocols :as bp]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-form :let [{:keys [bindings body path]} opts]
  (let [{:keys [compiled-bindings opts]} (b/compile-bindings bindings opts)

        compiled-body (compile-form body opts)

        deps (b/bindings-deps compiled-bindings compiled-body)

        let-sym (symbol path)]

    (reify fp/CompiledForm
      (form-deps [_] deps)

      (bindings [_]
        (concat (mapcat bp/bindings compiled-bindings)
                (fp/bindings compiled-body)

                (let [state (symbol (str path "-state"))
                      new-state (symbol (str path "-new-state"))
                      updated-vars (symbol (str path "-updated-vars"))]
                  
                  `[[~let-sym (reify fp/DynamicValue
                                (~'build [~'_ ~state]
                                  (let [~@(->> (mapcat #(bp/initial-bindings % state) compiled-bindings)
                                               (apply concat)
                                               (apply concat))]
                                    ~(fp/initial-value-form compiled-body state)))

                                (~'updated-value [~'_ ~new-state ~updated-vars]
                                  (let [~@(->> (mapcat #(bp/updated-bindings % new-state updated-vars) compiled-bindings)
                                               (apply concat)
                                               (apply concat))]
                                    ~(fp/updated-value-form compiled-body new-state updated-vars))))]])))

      (initial-value-form [_ state-sym]
        `(fp/build ~let-sym ~state-sym))

      (updated-value-form [_ new-state-sym updated-vars-sym]
        `(fp/updated-value ~let-sym ~new-state-sym ~updated-vars-sym)))))
