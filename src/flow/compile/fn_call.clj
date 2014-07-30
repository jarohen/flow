(ns flow.compile.fn-call
  (:require [flow.compile.calls :refer [compile-call-el compile-call-value]]
            [flow.compile :refer [compile-el]]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-el :fn-call [{:keys [path args]} opts]
  (let [compiled-args (map #(compile-el % opts) args)
        deps (set (mapcat fp/elem-deps compiled-args))
          
        !last-call-result (symbol (str "!" path))]

    (reify fp/CompiledElement
      (elem-deps [_] deps)
        
      (bindings [_]
        (concat (mapcat fp/bindings compiled-args)
                `[[~!last-call-result (atom nil)]]))

      (initial-el-form [_ state-sym]
        `(let [initial-result# (~@(map #(fp/initial-el-form % state-sym) compiled-args))]
           (reset! ~!last-call-result initial-result#)
           initial-result#))

      (updated-el-form [_ new-state-sym updated-vars-sym]
        (u/with-updated-deps-check deps updated-vars-sym
          `(let [new-result# (~@(map #(fp/updated-el-form % new-state-sym updated-vars-sym) compiled-args))]
             (reset! ~!last-call-result new-result#)
             new-result#)

          `@~!last-call-result)))))


(defmethod compile-call-value :fn-call [form opts])
