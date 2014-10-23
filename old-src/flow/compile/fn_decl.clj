(ns flow.compile.fn-decl
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.compile :refer [compile-identity compile-value]]
            [flow.bindings :as b]
            [flow.protocols :as fp]
            [clojure.set :as set]))

(alias 'fs (doto 'flow.state create-ns))

(defn with-arg-syms-removed [opts args]
  (let [bind-syms (set (mapcat b/destructuring-bind-syms args))]
    
    (-> opts
        (update-in [:dynamic-syms] set/difference bind-syms)
        (update-in [:local-syms] set/difference bind-syms))))

(defn compile-arity [{:keys [args body]} opts]
  (let [compiled-body (compile-value body (-> opts
                                              (with-arg-syms-removed args)))]
    (reify fp/CompiledValue
      (value-deps [_] (fp/value-deps compiled-body))

      (inline-value-form [_]
        `(~args ~(fp/inline-value-form compiled-body))))))

(defmethod compile-call-value :fn-decl [{:keys [path fn-name arities]} opts]
  (let [compiled-arities (map #(compile-arity % opts) arities)
        deps (set (mapcat fp/value-deps compiled-arities))]
    (reify fp/CompiledValue
      (value-deps [_] deps)

      (inline-value-form [_]
        `(fs/with-state-closure
           (fn ~@(when fn-name
                   [fn-name])
             ~@(map fp/inline-value-form compiled-arities)))))))
