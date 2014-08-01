(ns flow.compile.for
  (:require [flow.compile.calls :refer [compile-call-identity]]
            [flow.compile :refer [compile-identity]]
            [flow.bindings :as b]
            [flow.bindings.protocols :as bp]
            [flow.protocols :as fp]
            [flow.util :as u]))

(alias 'f (doto 'flow.core create-ns))

(defmethod compile-call-identity :for [{:keys [bindings body]} {:keys [path] :as opts}]
  (let [{:keys [compiled-bindings opts]} (b/compile-bindings bindings opts)

        compiled-body (compile-identity body opts)

        deps (b/bindings-deps compiled-bindings compiled-body)

        for-sym (u/path->sym path "for")]

    (reify fp/CompiledIdentity
      (identity-deps [_] deps)

      (bindings [_]
        (concat (mapcat bp/bindings compiled-bindings)
                

                (let [state (u/path->sym path "for" "state")
                      new-state (u/path->sym path "for" "new-state")
                      updated-vars (u/path->sym path "for" "updated-vars")]
                  
                  `[[~for-sym (let [!body-value-cache# (atom {})
                                    make-body# (fn []
                                                 (let [~@(apply concat (fp/bindings compiled-body))]
                                                   (reify fp/DynamicValue
                                                     (~'build [~'_ ~state]
                                                       ~(fp/initial-form compiled-body state))
                                                     (~'updated-value [~'_ ~new-state ~updated-vars]
                                                       ~(fp/updated-form compiled-body new-state updated-vars)))))]
                                
                                (reify fp/DynamicValue
                                  (~'build [~'_ ~state]
                                    (for [~@(->> (for [[value-bindings state-bindings] (map #(bp/initial-bindings % state) compiled-bindings)]
                                                   `[~@(apply concat value-bindings)
                                                     :let [~@(apply concat state-bindings)]])
                                                 (apply concat))]
                                      (let [body-keys# [~@(map bp/value-key compiled-bindings)]
                                            new-body# (make-body#)
                                            initial-value# (fp/build new-body# ~state)]
                                        (when (satisfies? fp/FlowElement initial-value#)
                                          (fp/set-flow-id! initial-value# body-keys#))
                                        
                                        (swap! !body-value-cache# assoc body-keys# new-body#)
                                        initial-value#)))

                                  (~'updated-value [~'_ ~new-state ~updated-vars]
                                    (let [old-cache# @!body-value-cache#
                                          !new-cache# (atom {})
                                          
                                          new-values# (for [~@(->> (for [[value-bindings state-bindings] (map #(bp/updated-bindings % new-state updated-vars) compiled-bindings)]
                                                                     `[~@(apply concat value-bindings)
                                                                       :let [~@(apply concat state-bindings)]])
                                                                   (apply concat))]
                                                        
                                                        (let [body-keys# [~@(map bp/value-key compiled-bindings)]
                                                              cached-body# (get old-cache# body-keys#)
                                                              [body# value#] (if cached-body#
                                                                               [cached-body# (fp/updated-value cached-body# ~new-state ~updated-vars)]
                                                                               (let [new-body# (make-body#)]
                                                                                 [new-body# (fp/build new-body# ~new-state)]))]
                                                          (swap! !new-cache# assoc body-keys# body#)
                                                          value#))]
                                      
                                      (reset! !body-value-cache# @!new-cache#)
                                      new-values#))))]])))

      (initial-form [_ state-sym]
        `(fp/build ~for-sym ~state-sym))

      (updated-form [_ new-state-sym updated-vars-sym]
        `(fp/updated-value ~for-sym ~new-state-sym ~updated-vars-sym)))))
