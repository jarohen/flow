(ns flow.compile.if
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.compile :refer [compile-identity compile-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

#_(defmethod compile-call-identity :if [{:keys [test then else]} {:keys [path] :as opts}]
    (let [compiled-test (compile-value test opts)
          compiled-then (compile-identity then (u/with-more-path opts ["then"]))
          compiled-else (compile-identity else (u/with-more-path opts ["else"]))

          deps (set (concat (fp/value-deps compiled-test)
                            (mapcat fp/identity-deps [compiled-then compiled-else])))

          !current-test-value (u/path->sym "!" path "current-test-value")
          !current-branch (u/path->sym "!" path "current-branch")
          !current-value (u/path->sym "!" path "current-value")

          build-then-branch (u/path->sym "build" path "then-branch")
          build-else-branch (u/path->sym "build" path "else-branch")

          state (gensym "state")
          new-state (gensym "new-state")
          updated-vars (gensym "updated-vars")]
      
      (letfn [(build-branch-fn [build-branch-sym compiled-branch]
                `(fn ~build-branch-sym []
                   (let [~@(apply concat (fp/bindings compiled-branch))]
                     (reify fp/DynamicValue
                       (~'build [~'_ ~state]
                         ~(fp/initial-form compiled-branch state))

                       (~'updated-value [~'_ ~new-state ~updated-vars]
                         ~(fp/updated-form compiled-branch new-state updated-vars))))))]
        
        (reify fp/CompiledIdentity
          (identity-deps [_] deps)

          (bindings [_]
            `[[~!current-test-value (atom nil)]
              [~!current-branch (atom nil)]
              [~!current-value (atom nil)]
              [~build-then-branch ~(build-branch-fn build-then-branch compiled-then)]
              [~build-else-branch ~(build-branch-fn build-else-branch compiled-else)]])

          (initial-form [_ state-sym]
            `(let [test-value# ~(fp/inline-value-form compiled-test state-sym)
                   initial-branch# (if test-value#
                                     (~build-then-branch)
                                     (~build-else-branch))
                   initial-value# (fp/build initial-branch# ~state-sym)]

               (reset! ~!current-test-value (boolean test-value#))
               (reset! ~!current-branch initial-branch#)
               (reset! ~!current-value initial-value#)
             
               initial-value#))

          (updated-form [_ new-state updated-vars]
            (u/with-updated-deps-check deps updated-vars
              `(let [old-test-value# @~!current-test-value
                     new-test-value# ~(fp/inline-value-form compiled-test new-state)]
               
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

(defmethod compile-call-value :if [{:keys [test then else]} opts]
  (let [[compiled-test compiled-then compiled-else] (map #(compile-value % opts) [test then else])

        deps (set (mapcat fp/value-deps [compiled-test compiled-then compiled-else]))]
      
    (reify fp/CompiledValue
      (value-deps [_] deps)

      (inline-value-form [_]
        `(if ~(fp/inline-value-form compiled-test)
           ~(fp/inline-value-form compiled-then)
           ~(fp/inline-value-form compiled-else))))))
