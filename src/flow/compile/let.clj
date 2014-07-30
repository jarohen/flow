(ns flow.compile.let
  (:require [flow.compile.calls :refer [compile-call-el]]
            [flow.compile :refer [compile-el]]
            [flow.bindings :as b]
            [flow.bindings.protocols :as bp]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-el :let [{:keys [bindings body]} {:keys [path] :as opts}]
  (let [{:keys [compiled-bindings opts]} (b/compile-bindings bindings opts)

        compiled-body (compile-el body (u/with-more-path opts ["let" "body"]))

        deps (b/bindings-deps compiled-bindings compiled-body)

        let-sym (u/path->sym path "let")]

    (reify fp/CompiledElement
      (elem-deps [_] deps)

      (bindings [_]
        (concat (mapcat bp/bindings compiled-bindings)
                (fp/bindings compiled-body)))

      (initial-el-form [_ state-sym]
        `(let [~@(->> (mapcat #(bp/initial-bindings % state-sym) compiled-bindings)
                      (apply concat)
                      (apply concat))]
           ~(fp/initial-el-form compiled-body state-sym)))

      (updated-el-form [_ new-state-sym updated-vars-sym]
        `(let [~@(->> (mapcat #(bp/updated-bindings % new-state-sym updated-vars-sym) compiled-bindings)
                      (apply concat)
                      (apply concat))]
           ~(fp/updated-el-form compiled-body new-state-sym updated-vars-sym))))))
