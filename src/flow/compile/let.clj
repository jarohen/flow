(ns flow.compile.let
  (:require [flow.compile.calls :refer [compile-call-identity]]
            [flow.compile :refer [compile-identity]]
            [flow.bindings :as b]
            [flow.bindings.protocols :as bp]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-identity :let [{:keys [bindings body]} {:keys [path] :as opts}]
  (let [{:keys [compiled-bindings opts]} (b/compile-bindings bindings opts)

        compiled-body (compile-identity body (u/with-more-path opts ["let" "body"]))

        deps (b/bindings-deps compiled-bindings compiled-body)

        let-sym (u/path->sym path "let")]

    (reify fp/CompiledIdentity
      (identity-deps [_] deps)

      (bindings [_]
        (concat (mapcat bp/bindings compiled-bindings)
                (fp/bindings compiled-body)))

      (initial-form [_ state-sym]
        `(let [~@(->> (mapcat #(bp/initial-bindings % state-sym) compiled-bindings)
                      (apply concat)
                      (apply concat))]
           ~(fp/initial-form compiled-body state-sym)))

      (updated-form [_ new-state-sym updated-vars-sym]
        `(let [~@(->> (mapcat #(bp/updated-bindings % new-state-sym updated-vars-sym) compiled-bindings)
                      (apply concat)
                      (apply concat))]
           ~(fp/updated-form compiled-body new-state-sym updated-vars-sym))))))
