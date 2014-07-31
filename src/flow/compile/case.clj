(ns flow.compile.case
  (:require [flow.compile.calls :refer [compile-call-el compile-call-value]]
            [flow.compile :refer [compile-el compile-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

#_(defmethod compile-call-el :case [{:keys [test then else]} {:keys [path] :as opts}]
    (let [compiled-test (compile-value test opts)
          compiled-then (compile-el then (u/with-more-path opts ["then"]))
          compiled-else (compile-el else (u/with-more-path opts ["else"]))

          deps (set (concat (fp/value-deps compiled-test)
                            (mapcat fp/elem-deps [compiled-then compiled-else])))

          !current-test-value (u/path->sym "!" path "current-test-value")
          !current-branch (u/path->sym "!" path "current-branch")
          !current-value (u/path->sym "!" path "current-value")

          build-then-branch (u/path->sym "build" path "then-branch")
          build-else-branch (u/path->sym "build" path "else-branch")

          state (gensym "state")
          new-state (gensym "new-state")
          updated-vars (gensym "updated-vars")]
      
      (letfn [(build-branch-fn [build-clause-sym compiled-clause]
                `(fn ~build-clause-sym []
                   (let [~@(apply concat (fp/bindings compiled-clause))]
                     (reify fp/DynamicValue
                       (~'build [~'_ ~state]
                         ~(fp/initial-el-form compiled-clause state))

                       (~'updated-value [~'_ ~new-state ~updated-vars]
                         ~(fp/updated-el-form compiled-clause new-state updated-vars))))))]
        
        (reify fp/CompiledElement
          (elem-deps [_] deps)

          (bindings [_]
            `[[~!current-test-value (atom nil)]
              [~!current-branch (atom nil)]
              [~!current-value (atom nil)]
              [~build-then-branch ~(build-branch-fn build-then-branch compiled-then)]
              [~build-else-branch ~(build-branch-fn build-else-branch compiled-else)]])

          (initial-el-form [_ state-sym]
            `(let [test-value# ~(fp/inline-value-form compiled-test state-sym)
                   initial-branch# (if test-value#
                                     (~build-then-branch)
                                     (~build-else-branch))
                   initial-value# (fp/build initial-branch# ~state-sym)]

               (reset! ~!current-test-value (boolean test-value#))
               (reset! ~!current-branch initial-branch#)
               (reset! ~!current-value initial-value#)
             
               initial-value#))

          (updated-el-form [_ new-state updated-vars]
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

(defmethod compile-call-value :case [{:keys [case-expr cases default]} opts]
  (let [compiled-case-expr (compile-value case-expr opts)
        compiled-cases (map #(update-in % [:expr] compile-value opts) cases)
        compiled-default (when default
                           (compile-value default opts))

        deps (set (mapcat fp/value-deps (concat [compiled-case-expr]
                                                (map :expr compiled-cases)
                                                [compiled-default])))]

    (reify fp/CompiledValue
      (value-deps [_] deps)

      (inline-value-form [_ state-sym]
        `(case ~(fp/inline-value-form compiled-case-expr state-sym)

           ~@(->> (for [{:keys [test expr]} compiled-cases]
                    [test (fp/inline-value-form expr state-sym)])
                  (apply concat))

           ~@(when compiled-default
               [(fp/inline-value-form compiled-default state-sym)]))))))
