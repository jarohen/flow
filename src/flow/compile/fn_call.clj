(ns flow.compile.fn-call
  (:require [flow.compile :refer [compile-form]]
            [flow.compile.calls :refer [compile-call-form]]
            [flow.util :as u]
            [flow.protocols :as fp]))

(defmethod compile-call-form :fn-call [{:keys [path args]} opts]
  (let [compiled-args (map #(compile-form % opts) args)
        deps (set (mapcat fp/form-deps compiled-args))
          
        !last-call-result (symbol (str "!" path))]

    (assert (empty? deps) "I don't handle this case yet")
    
    (reify fp/CompiledForm
      (form-deps [_] deps)
        
      (bindings [_]
        (concat (mapcat fp/bindings compiled-args)
                `[[~!last-call-result (atom nil)]]))

      (initial-value-form [_ state-sym]
        ;; TODO when deps?
        (when-not (seq deps)
          `(~@(map #(fp/initial-value-form % state-sym) compiled-args))))

      (updated-value-form [_ old-state-sym new-state-sym updated-vars-sym]
        `(if (u/deps-updated? ~(u/quote-deps deps) ~updated-vars-sym)
           (let [new-args# (map second )]
             )

           (let [last-result# @~!last-call-result]
             [last-result# last-result#]))))))


