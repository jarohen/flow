(ns flow.compile.case
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.compile :refer [compile-identity compile-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-identity :case [{:keys [case-expr clauses default]} {:keys [path] :as opts}]
  (let [path (concat path ["case"])

        compiled-case-expr (compile-value case-expr (u/with-more-path opts ["case" "expr"]))
        compiled-clauses (map #(assoc %
                                 :compiled-clause (compile-identity (:expr %) (u/with-more-path opts ["case" (str (:idx %))]))
                                 :builder-sym (u/path->sym "build" path (str (:idx %))))
                              clauses)
        compiled-default (when default
                           {:compiled-clause (compile-identity default (u/with-more-path opts ["case" "default"]))
                            :builder-sym (u/path->sym "build" path "default")})

        deps (set (concat (fp/value-deps compiled-case-expr)
                          (mapcat (comp fp/identity-deps :compiled-clause) compiled-clauses)
                          (fp/identity-deps (:compiled-clause compiled-default))))

        !current-expr-value (u/path->sym "!" path "current-expr-value")
        !current-clause (u/path->sym "!" path "current-clause")
        !current-value (u/path->sym "!" path "current-value")

        current-clause (u/path->sym path "current-clause")
        
        state (gensym "state")
        new-state (gensym "new-state")
        updated-vars (gensym "updated-vars")]
      
    (letfn [(build-clause-fn [build-clause-sym compiled-clause]
              `(fn ~build-clause-sym []
                 (let [~@(apply concat (fp/bindings compiled-clause))]
                   (reify fp/DynamicValue
                     (~'build [~'_ ~state]
                       ~(fp/initial-form compiled-clause state))

                     (~'updated-value [~'_ ~new-state ~updated-vars]
                       ~(fp/updated-form compiled-clause new-state updated-vars))))))]
        
      (reify fp/CompiledIdentity
        (identity-deps [_] deps)

        (bindings [_]
          `[[~!current-expr-value (atom nil)]
            [~!current-clause (atom nil)]
            [~!current-value (atom nil)]

            ~@(for [{:keys [compiled-clause builder-sym]} (concat compiled-clauses
                                                                  (when compiled-default
                                                                    [compiled-default]))]
                [builder-sym (build-clause-fn builder-sym compiled-clause)])

            [~current-clause (fn [expr-value#]
                               (case expr-value#
                                 ~@(->> (for [{:keys [test builder-sym]} compiled-clauses]
                                          [test `(~builder-sym)])
                                        (apply concat))

                                 ~@(when-let [{:keys [builder-sym]} compiled-default]
                                     [`(~builder-sym)])))]])

        (initial-form [_ state-sym]
          `(let [expr-value# ~(fp/inline-value-form compiled-case-expr state-sym)
                 initial-clause# (~current-clause expr-value#)
                 initial-value# (fp/build initial-clause# ~state-sym)]

             (reset! ~!current-expr-value expr-value#)
             (reset! ~!current-clause initial-clause#)
             (reset! ~!current-value initial-value#)
             
             initial-value#))

        (updated-form [_ new-state-sym updated-vars-sym]
          (u/with-updated-deps-check deps updated-vars-sym
            `(let [old-expr-value# @~!current-expr-value
                   new-expr-value# ~(fp/inline-value-form compiled-case-expr new-state-sym)]
               
               (if (not= old-expr-value# new-expr-value#)
                 (let [new-clause# (~current-clause new-expr-value#)
                       new-value# (fp/build new-clause# ~new-state-sym)]
                   (reset! ~!current-expr-value new-expr-value#)
                   (reset! ~!current-clause new-clause#)
                   (reset! ~!current-value new-value#)
                   new-value#)

                 (let [new-value# (fp/updated-value @~!current-clause
                                                    ~new-state-sym
                                                    ~updated-vars-sym)]
                   (reset! ~!current-value new-value#)
                   new-value#)))

            `@~!current-value))))))

(defmethod compile-call-value :case [{:keys [case-expr clauses default]} opts]
  (let [compiled-case-expr (compile-value case-expr opts)
        compiled-clauses (map #(update-in % [:expr] compile-value opts) clauses)
        compiled-default (when default
                           (compile-value default opts))

        deps (set (mapcat fp/value-deps (concat [compiled-case-expr]
                                                (map :expr compiled-clauses)
                                                [compiled-default])))]

    (reify fp/CompiledValue
      (value-deps [_] deps)

      (inline-value-form [_ state-sym]
        `(case ~(fp/inline-value-form compiled-case-expr state-sym)

           ~@(->> (for [{:keys [test expr]} compiled-clauses]
                    [test (fp/inline-value-form expr state-sym)])
                  (apply concat))

           ~@(when compiled-default
               [(fp/inline-value-form compiled-default state-sym)]))))))
