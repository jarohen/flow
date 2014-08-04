(ns flow.compile.fn-decl
  (:require [flow.compile.calls :refer [compile-call-form]]
            [flow.compile :refer [compile-form]]
            [flow.bindings :as b]
            [flow.protocols :as fp]
            [clojure.set :as set]))

(defn compile-arity [{:keys [path args body]} opts]
  (let [compiled-body (compile-form body (update-in opts [:dynamic-syms]
                                                    set/difference
                                                    (set (mapcat b/destructuring-bind-syms args))))]
    (reify fp/CompiledForm
      (form-deps [_] (fp/form-deps compiled-body))

      (bindings [_] (fp/bindings compiled-body))

      (initial-value-form [_ state-sym]
        `(~args ~(fp/initial-value-form compiled-body state-sym)))

      (updated-value-form [_ new-state-sym updated-vars-sym]
        ;; TODO
        ))))

(defmethod compile-call-form :fn-decl [{:keys [path fn-name arities]} opts]
  (let [compiled-arities (map #(compile-arity % opts) arities)
        deps (set (mapcat fp/form-deps compiled-arities))]
    (reify fp/CompiledForm
      (form-deps [_] deps)

      (bindings [_]
        (mapcat fp/bindings compiled-arities))

      (initial-value-form [_ state-sym]
        `(fn ~@(when fn-name
                 [fn-name])
           ~@(map #(fp/initial-value-form % state-sym) compiled-arities)))

      (updated-value-form [_ new-state-sym updated-vars-sym]
        ;; TODO
        ))))
