(ns flow.compile.case
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.compile :refer [compile-identity compile-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-identity :case [{:keys [case-expr clauses default]} {:keys [path] :as opts}]
  (let [compiled-case-expr (compile-value case-expr (u/with-more-path opts ["case" "expr"]))
        compiled-clause-pairs (map #(compile-identity (:expr %)
                                                      (u/with-more-path opts ["case" (str (:idx %))]))
                                   clauses)
        compiled-default (when default
                           (compile-identity default
                                             (u/with-more-path opts ["case" "default"])))

        compiled-clauses (concat compiled-clause-pairs
                                 (when compiled-default
                                   [compiled-default]))

        case-sym (u/path->sym path "case")]
      
    (reify fp/CompiledIdentity
      (hard-deps [_] (fp/value-deps compiled-case-expr))
      (soft-deps [_] (set (concat (mapcat fp/hard-deps compiled-clauses)
                                  (mapcat fp/soft-deps compiled-clauses))))

      (declarations [_]
        (concat (mapcat fp/declarations compiled-clauses)
                (letfn [(clause->map [compiled-clause]
                          {:deps (u/quote-deps (concat (fp/hard-deps compiled-clause)
                                                       (fp/soft-deps compiled-clause)))
                           :build-fn `(fn []
                                        ~(fp/build-form compiled-clause))})]
                  
                  `[(defn ~case-sym []
                      (flow.forms.case/build-case
                       {:deps ~(u/quote-deps (fp/value-deps compiled-case-expr))
                        :value-fn (fn []
                                    ~(fp/inline-value-form compiled-case-expr))}

                       (fn [expr#]
                         (case expr#
                           ~@(->> (for [[{:keys [test]} compiled-clause] (map vector clauses compiled-clauses)]
                                    [test (clause->map compiled-clause)])
                                  (apply concat))

                           ~@(when default
                               [(clause->map compiled-default)])))))])))

      (build-form [_]
        `(~case-sym)))))

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

      (inline-value-form [_]
        `(case ~(fp/inline-value-form compiled-case-expr)

           ~@(->> (for [{:keys [test expr]} compiled-clauses]
                    [test (fp/inline-value-form expr)])
                  (apply concat))

           ~@(when compiled-default
               [(fp/inline-value-form compiled-default)]))))))
