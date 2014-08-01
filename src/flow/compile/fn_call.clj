(ns flow.compile.fn-call
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.compile :refer [compile-identity compile-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-identity :fn-call [{:keys [args]} {:keys [path] :as opts}]
  (let [compiled-args (map #(compile-identity % opts) args)
        deps (set (mapcat fp/identity-deps compiled-args))

        !current-args (u/path->sym "!" path "current-args")
        !current-value (u/path->sym "!" path "current-value")]

    (reify fp/CompiledIdentity
      (identity-deps [_] deps)
        
      (bindings [_]
        (concat (mapcat fp/bindings compiled-args)
                
                `[[~!current-args (atom nil)]
                  [~!current-value (atom nil)]]))
      
      (initial-form [_ state-sym]
        `(let [initial-args# [~@(map #(fp/initial-form % state-sym) compiled-args)]
               initial-value# (apply (first initial-args#) (rest initial-args#))]
           (reset! ~!current-args initial-args#)
           (reset! ~!current-value initial-value#)
           initial-value#))

      (updated-form [_ new-state-sym updated-vars-sym]
        (u/with-updated-deps-check deps updated-vars-sym
          `(let [new-args# [~@(map #(fp/updated-form % new-state-sym updated-vars-sym) compiled-args)]
                 new-value# (if-not (every? true? (map = new-args# @~!current-args))
                              (apply (first new-args#) (rest new-args#))
                              @~!current-value)]
             (reset! ~!current-args new-args#)
             (reset! ~!current-value new-value#)
             new-value#)
          
          `@~!current-value)))))


(defmethod compile-call-value :fn-call [{:keys [args]} opts]
  (let [compiled-args (map #(compile-value % opts) args)]

    (reify fp/CompiledValue
      (value-deps [_] (set (mapcat fp/value-deps compiled-args)))
        
      (inline-value-form [_ state-sym]
        `(~@(map #(fp/inline-value-form % state-sym) compiled-args))))))
