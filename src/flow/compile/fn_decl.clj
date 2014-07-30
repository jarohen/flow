(ns flow.compile.fn-decl
  (:require [flow.compile.calls :refer [compile-call-el compile-call-value]]
            [flow.compile :refer [compile-el]]
            [flow.bindings :as b]
            [flow.protocols :as fp]
            [clojure.set :as set]))

(defn with-arg-syms-removed [syms args]
  (let [bind-syms (set (mapcat b/destructuring-bind-syms args))]
    
    (-> opts
        (update-in [:dynamic-syms] set/difference bind-syms)
        (update-in [:local-syms] set/difference bind-syms))))

(defn compile-arity [{:keys [path args body]} opts]
  (let [compiled-body (compile-el body (-> opts
                                           (with-arg-syms-removed args)))]
    (reify fp/CompiledElement
      (elem-deps [_] (fp/elem-deps compiled-body))

      (bindings [_] (fp/bindings compiled-body))

      (initial-el-form [_ state-sym]
        `(~args ~(fp/initial-el-form compiled-body state-sym)))

      (updated-el-form [_ new-state-sym updated-vars-sym]
        `(~args ~(fp/initial-el-form compiled-body new-state-sym updated-vars-sym))))))

(defmethod compile-call-value :fn-decl [{:keys [path fn-name arities]} opts]
  (let [compiled-arities (map #(compile-arity % opts) arities)
        deps (set (mapcat fp/elem-deps compiled-arities))]
    (reify fp/CompiledElement
      (elem-deps [_] deps)

      (bindings [_]
        (mapcat fp/bindings compiled-arities))

      (initial-el-form [_ state-sym]
        `(fn ~@(when fn-name
                 [fn-name])
           ~@(map #(fp/initial-el-form % state-sym) compiled-arities)))

      (updated-el-form [_ new-state-sym updated-vars-sym]
        `(fn ~@(when fn-name
                 [fn-name])
           ~@(map #(fp/updated-el-form % new-state-sym updated-vars-sym) compiled-arities))))))
