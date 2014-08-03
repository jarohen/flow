(ns flow.compile.let
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.compile :refer [compile-identity]]
            [flow.bindings :as b]
            [flow.protocols :as fp]
            [flow.util :as u]
            [clojure.set :as set]))

(defmethod compile-call-identity :let [{:keys [bindings body]} {:keys [path] :as opts}]
  (let [{:keys [compiled-bindings opts]} (b/compile-bindings bindings opts)

        compiled-body (compile-identity body (u/with-more-path opts ["let" "body"]))

        {:keys [hard-deps soft-deps]} (b/bindings-deps compiled-bindings compiled-body)

        let-sym (u/path->sym path "let")]

    (reify fp/CompiledIdentity
      (hard-deps [_] hard-deps)
      (soft-deps [_] soft-deps)

      (declarations [_]
        (concat (mapcat :declarations compiled-bindings)
                (fp/declarations compiled-body)
                `[(defn ~let-sym []
                    (flow.forms.let/build-let [~@(map #(-> %
                                                           (dissoc :declarations :bound-syms :key-fn)
                                                           (update-in [:hard-deps] u/quote-deps)
                                                           (update-in [:soft-deps] u/quote-deps))
                                                      compiled-bindings)]
                                   
                                              {:deps ~(u/quote-deps (set/union (fp/hard-deps compiled-body)
                                                                               (fp/soft-deps compiled-body)))
                                               :build-fn (fn []
                                                           ~(fp/build-form compiled-body))}))]))

      (build-form [_]
        `(~let-sym)))))

(defmethod compile-call-value :let [{:keys [bindings body]} opts]
  ;; TODO going to require some changes to bindings (or a different
  ;; compile-bindings fn) because 'compile-bindings' currently expects
  ;; this to be an identity
  (throw (Exception. "not implemented")))
