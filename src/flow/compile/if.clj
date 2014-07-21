(ns flow.compile.if
  (:require [flow.compile.calls :refer [compile-call-form]]
            [flow.compile :refer [compile-form]]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-form :if [{:keys [path test then else]} {:keys [state] :as opts}]
  (let [[compiled-test compiled-then compiled-else] (map #(compile-form % opts) [test then else])

        deps (set (mapcat fp/form-deps [compiled-test compiled-then compiled-else]))

        !current-test-value (symbol (str "!" path "-current-test-value"))
        !current-branch (symbol (str "!" path "-current-branch"))
        !current-value (symbol (str "!" path "-current-value"))

        build-then-branch (symbol (str "build-" path "-then-branch"))
        build-else-branch (symbol (str "build-" path "-else-branch"))

        state (gensym "state")
        new-state (gensym "new-state")
        updated-vars (gensym "updated-vars")]
      
    (letfn [(build-branch-fn [build-branch-sym compiled-branch]
              `(fn ~build-branch-sym []
                 (let [~@(apply concat (fp/bindings compiled-branch))]
                   (reify fp/DynamicValue
                     (~'build [~'_ ~state]
                       ~(fp/initial-value-form compiled-branch state))

                     (~'updated-value [~'_ ~new-state ~updated-vars]
                       ~(fp/updated-value-form compiled-branch
                                               new-state
                                               updated-vars))))))]
        
      (reify fp/CompiledForm
        (form-deps [_] deps)

        (bindings [_]
          (concat (fp/bindings compiled-test)
                  `[[~!current-test-value (atom nil)]
                    [~!current-branch (atom nil)]
                    [~!current-value (atom nil)]
                    [~build-then-branch ~(build-branch-fn build-then-branch compiled-then)]
                    [~build-else-branch ~(build-branch-fn build-else-branch compiled-else)]]))

        (initial-value-form [_ state-sym]
          `(let [test-value# ~(fp/initial-value-form compiled-test state-sym)
                 initial-branch# (if test-value#
                                   (~build-then-branch)
                                   (~build-else-branch))
                 initial-value# (fp/build initial-branch# ~state-sym)]

             (reset! ~!current-test-value (boolean test-value#))
             (reset! ~!current-branch initial-branch#)
             (reset! ~!current-value initial-value#)
             
             initial-value#))

        (updated-value-form [_ new-state updated-vars]
          (u/with-updated-deps-check deps updated-vars
            `(let [old-test-value# @~!current-test-value
                   new-test-value# ~(fp/updated-value-form compiled-test
                                                           new-state
                                                           updated-vars)]
               
               (if (not= (boolean old-test-value#) (boolean new-test-value#))
                 (let [new-branch# (if new-test-value#
                                     (~build-then-branch)
                                     (~build-else-branch))
                       new-value# (fp/build new-branch# ~new-state)]

                   (reset! ~!current-test-value new-test-value#)
                   (reset! ~!current-branch new-branch#)
                   (reset! ~!current-value new-value#)
                   new-value#)

                 (let [new-value# (fp/updated-value @~!current-branch
                                                    ~new-state
                                                    ~updated-vars)]
                   (reset! ~!current-value new-value#)
                   new-value#)))

            `@~!current-value))))))



