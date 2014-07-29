(ns flow.compile.unwrap-cursor
  (:require [flow.compile.calls :refer [compile-call-el compile-call-value]]
            [flow.protocols :as fp]))

(defmethod compile-call-value :unwrap-cursor [{:keys [cursor]} opts]
  (reify fp/CompiledForm
    (form-deps [_] #{cursor})

    (bindings [_] nil)

    (initial-value-form [_ state-sym]
      `(get ~state-sym (quote ~cursor)))

    (updated-value-form [_ new-state-sym updated-vars-sym]
      `(get ~new-state-sym (quote ~cursor)))))

(defmethod compile-call-el :unwrap-cursor [form opts]
  (compile-call-value form opts))
