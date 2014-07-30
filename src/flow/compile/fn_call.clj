(ns flow.compile.fn-call
  (:require [flow.compile.calls :refer [compile-call-el compile-call-value]]
            [flow.compile :refer [compile-el compile-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-el :fn-call [{:keys [args]} {:keys [path] :as opts}]
  (let [compiled-args (map #(compile-value % opts) args)]

    (reify fp/CompiledElement
      (elem-deps [_] (set (mapcat fp/value-deps compiled-args)))
        
      (bindings [_])
      
      (initial-el-form [_ state-sym]
        `(~@(map #(fp/inline-value-form % state-sym) compiled-args)))

      (updated-el-form [_ new-state-sym updated-vars-sym]
        `(~@(map #(fp/inline-value-form % new-state-sym) compiled-args))))))


(defmethod compile-call-value :fn-call [{:keys [args]} opts]
  (let [compiled-args (map #(compile-value % opts) args)]

    (reify fp/CompiledValue
      (value-deps [_] (set (mapcat fp/value-deps compiled-args)))
        
      (inline-value-form [_ state-sym]
        `(~@(map #(fp/inline-value-form % state-sym) compiled-args))))))
