(ns flow.compile.for
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.compile :refer [compile-identity]]
            [flow.bindings :as b]
            [flow.protocols :as fp]
            [flow.util :as u]
            [clojure.set :as set]))

(alias 'f (doto 'flow.core create-ns))

(defmethod compile-call-identity :for [{:keys [bindings body]} {:keys [path] :as opts}]
  (let [{:keys [compiled-bindings opts]} (b/compile-bindings bindings opts)

        compiled-body (compile-identity body (u/with-more-path opts ["for" "body"]))

        {:keys [hard-deps soft-deps]} (b/bindings-deps compiled-bindings compiled-body)

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
  ;; TODO going to require some changes to bindings (or a different
  ;; compile-bindings fn) because 'compile-bindings' currently expects
  ;; this to be an identity
  (throw (Exception. "not implemented")))
