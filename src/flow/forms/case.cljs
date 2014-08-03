(ns flow.forms.case
  (:require [flow.util :as u]))

(defn build-case [compiled-case-expr expr->compiled-clause]
  (letfn [(update-case [current-expr-value current-clause current-value update-fn]
            (letfn [(update-clause []
                      (if (u/deps-updated? (:deps current-clause))
                        (let [[new-value new-update-fn] (update-fn)]
                          [new-value (update-case current-expr-value
                                                  current-clause
                                                  new-value
                                                  new-update-fn)])

                        [current-expr-value (update-case current-expr-value
                                                         current-clause
                                                         current-expr-value
                                                         update-fn)]))]
              (fn []
                (if (u/deps-updated? (:deps compiled-case-expr))
                  (let [new-expr-value ((:value-fn compiled-case-expr))]
                    (if (not= current-expr-value new-expr-value)
                      (let [new-clause (expr->compiled-clause new-expr-value)
                            [new-value new-update-fn] ((:build-fn new-clause))]
                        [new-value (update-case new-expr-value new-clause new-value new-update-fn)])
                    
                      (update-clause)))

                  (update-clause)))))]
    
    (let [expr-value ((:value-fn compiled-case-expr))
          initial-clause (expr->compiled-clause expr-value)
          [initial-value update-fn] ((:build-fn initial-clause))]
      [initial-value (update-case expr-value initial-clause initial-value update-fn)])))

