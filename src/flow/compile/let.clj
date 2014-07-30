(ns flow.compile.let
  (:require [flow.compile.calls :refer [compile-call-el]]
            [flow.compile :refer [compile-el]]
            [flow.bindings :as b]
            [flow.bindings.protocols :as bp]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-el :let [{:keys [bindings body]} {:keys [path] :as opts}]
  (let [{:keys [compiled-bindings opts]} (b/compile-bindings bindings opts)

        compiled-body (compile-el body (u/with-more-path opts ["let" "body"]))

        deps (b/bindings-deps compiled-bindings compiled-body)

        let-sym (u/path->sym path "let")]

    (reify fp/CompiledElement
      (elem-deps [_] deps)

      (bindings [_]
        (concat (mapcat bp/bindings compiled-bindings)
                (fp/bindings compiled-body)

                (let [state (u/path->sym path "state")
                      new-state (u/path->sym path "new-state")
                      updated-vars (u/path->sym path "updated-vars")]
                  
                  `[[~let-sym (reify fp/DynamicValue
                                (~'build [~'_ ~state]
                                  (let [~@(->> (mapcat #(bp/initial-bindings % state) compiled-bindings)
                                               (apply concat)
                                               (apply concat))]
                                    ~(fp/initial-el-form compiled-body state)))

                                (~'updated-value [~'_ ~new-state ~updated-vars]
                                  (let [~@(->> (mapcat #(bp/updated-bindings % new-state updated-vars) compiled-bindings)
                                               (apply concat)
                                               (apply concat))]
                                    ~(fp/updated-el-form compiled-body new-state updated-vars))))]])))

      (initial-el-form [_ state-sym]
        `(fp/build ~let-sym ~state-sym))

      (updated-el-form [_ new-state-sym updated-vars-sym]
        `(fp/updated-value ~let-sym ~new-state-sym ~updated-vars-sym)))))
