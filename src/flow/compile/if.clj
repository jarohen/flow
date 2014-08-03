(ns flow.compile.if
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.compile :refer [compile-identity compile-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-identity :if [{:keys [test then else]} {:keys [path] :as opts}]
  (let [compiled-test (compile-value test opts)
        compiled-then (compile-identity then (u/with-more-path opts ["then"]))
        compiled-else (compile-identity else (u/with-more-path opts ["else"]))

        if-sym (u/path->sym path "if")]
      
    (reify fp/CompiledIdentity
      (hard-deps [_] (set (concat (fp/value-deps compiled-test)
                                  (mapcat fp/hard-deps [compiled-then compiled-else]))))
      (soft-deps [_] (set (mapcat fp/soft-deps [compiled-then compiled-else])))

      (declarations [_]
        (concat (mapcat fp/declarations [compiled-then compiled-else])
                (letfn [(branch->map [compiled-branch]
                          {:deps (u/quote-deps (concat (fp/hard-deps compiled-branch)
                                                       (fp/soft-deps compiled-branch)))
                           :build-fn `(fn []
                                        ~(fp/build-form compiled-branch))})]
                  `[(defn ~if-sym []
                      (flow.forms.if/build-if
                       {:deps ~(u/quote-deps (fp/value-deps compiled-test))
                        :value-fn (fn []
                                    ~(fp/inline-value-form compiled-test))}
                       
                       ~(branch->map compiled-then)
                       ~(branch->map compiled-else)))])))
      
      (build-form [_]
        `(~if-sym)))))

(defmethod compile-call-value :if [{:keys [test then else]} opts]
  (let [[compiled-test compiled-then compiled-else] (map #(compile-value % opts) [test then else])

        deps (set (mapcat fp/value-deps [compiled-test compiled-then compiled-else]))]
      
    (reify fp/CompiledValue
      (value-deps [_] deps)

      (inline-value-form [_]
        `(if ~(fp/inline-value-form compiled-test)
           ~(fp/inline-value-form compiled-then)
           ~(fp/inline-value-form compiled-else))))))
