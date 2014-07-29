(ns flow.compile.for
  (:require [flow.compile.calls :refer [compile-call-el]]
            [flow.compile :refer [compile-el]]
            [flow.bindings :as b]
            [flow.bindings.protocols :as bp]
            [flow.protocols :as fp]
            [flow.util :as u]))

(alias 'f (doto 'flow.core create-ns))

(defmethod compile-call-el :for [{:keys [bindings body path]} opts]
  (let [{:keys [compiled-bindings opts]} (b/compile-bindings bindings opts)

        compiled-body (compile-el body opts)

        deps (b/bindings-deps compiled-bindings compiled-body)

        for-sym (symbol path)]

    (reify fp/CompiledForm
      (form-deps [_] deps)

      (bindings [_]
        (concat (mapcat bp/bindings compiled-bindings)
                

                (let [state (symbol (str path "-state"))
                      new-state (symbol (str path "-new-state"))
                      updated-vars (symbol (str path "-updated-vars"))]
                  
                  `[[~for-sym (let [!body-value-cache# (atom {})
                                    make-body# (fn []
                                                 (let [~@(apply concat (fp/bindings compiled-body))]
                                                   (reify fp/DynamicValue
                                                     (~'build [~'_ ~state]
                                                       ~(fp/initial-value-form compiled-body state))
                                                     (~'updated-value [~'_ ~new-state ~updated-vars]
                                                       ~(fp/updated-value-form compiled-body new-state updated-vars)))))]
                                
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

      (initial-value-form [_ state-sym]
        `(fp/build ~for-sym ~state-sym))

      (updated-value-form [_ new-state-sym updated-vars-sym]
        `(fp/updated-value ~for-sym ~new-state-sym ~updated-vars-sym)))))
