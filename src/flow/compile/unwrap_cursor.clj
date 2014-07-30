(ns flow.compile.unwrap-cursor
  (:require [flow.compile.calls :refer [compile-call-el compile-call-value]]
            [flow.protocols :as fp]))

(defmethod compile-call-value :unwrap-cursor [{:keys [cursor]} opts]
  (reify fp/CompiledElement
    (elem-deps [_] #{cursor})

    (bindings [_] nil)

    (initial-el-form [_ state-sym]
      `(get ~state-sym (quote ~cursor)))

    (updated-el-form [_ new-state-sym updated-vars-sym]
      `(get ~new-state-sym (quote ~cursor)))))

(defmethod compile-call-el :unwrap-cursor [form opts]
  (compile-call-value form opts))
