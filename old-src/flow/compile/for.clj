(ns flow.compile.for
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.compile :refer [compile-identity compile-value]]
            [flow.bindings :as b]
            [flow.protocols :as fp]
            [flow.util :as u]
            [clojure.set :as set]))

(alias 'fs (doto 'flow.state create-ns))

(defmethod compile-call-identity :for [{:keys [bindings body]} {:keys [path] :as opts}]
  (let [{:keys [compiled-bindings opts]} (b/compile-identity-bindings bindings opts)

        compiled-body (compile-identity body (u/with-more-path opts ["for" "body"]))

        {:keys [hard-deps soft-deps]} (b/identity-bindings-deps compiled-bindings compiled-body)

        for-sym (u/path->sym path "for")]

    (reify fp/CompiledIdentity
      (hard-deps [_] hard-deps)
      (soft-deps [_] soft-deps)

      (declarations [_]
        (concat (mapcat :declarations compiled-bindings)
                (fp/declarations compiled-body)
                `[(defn ~for-sym []
                    (flow.forms.for/build-for ~(u/quote-syms (set (concat hard-deps soft-deps)))
                                              
                                              [~@(map #(-> %
                                                           (dissoc :declarations)
                                                           (update-in [:bound-syms] u/quote-syms)
                                                           (update-in [:hard-deps] u/quote-syms)
                                                           (update-in [:soft-deps] u/quote-syms)
                                                           (update-in [:deps] u/quote-syms))
                                                      compiled-bindings)]
                                   
                                              {:deps ~(u/quote-syms (set/union (fp/hard-deps compiled-body)
                                                                               (fp/soft-deps compiled-body)))
                                               :build-fn (fn []
                                                           ~(fp/build-form compiled-body))}))]))

      (build-form [_]
        `(~for-sym)))))

(defmethod compile-call-value :for [{:keys [bindings body]} opts]
  (let [state-sym (gensym "state")

        {:keys [compiled-bindings opts]} (b/compile-value-bindings bindings (assoc opts
                                                                              :state-sym state-sym))
        compiled-body (compile-value body opts)]

    (reify fp/CompiledValue
      (value-deps [_] (b/value-bindings-deps compiled-bindings compiled-body))

      (inline-value-form [_]
        `(let [~state-sym fs/*state*]
           (for [~@(->> (for [{:keys [value-bindings state-bindings]} compiled-bindings]
                          `[~@(apply concat value-bindings)
                            :let [~@(apply concat state-bindings)]])
                        (apply concat))]
             (binding [fs/*state* ~state-sym]
               ~(fp/inline-value-form compiled-body))))))))
