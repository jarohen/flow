(ns flow.compile.unwrap-cursor
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.protocols :as fp]))

(defmethod compile-call-identity :unwrap-cursor [{:keys [cursor]} opts]
  (reify fp/CompiledIdentity
    (identity-deps [_] #{cursor})

    (bindings [_] nil)

    (initial-form [_ state-sym]
      `(get ~state-sym (quote ~cursor)))

    (updated-form [_ new-state-sym updated-vars-sym]
      `(get ~new-state-sym (quote ~cursor)))))

(defmethod compile-call-value :unwrap-cursor [{:keys [cursor]} opts]
  (reify fp/CompiledValue
    (value-deps [_] #{cursor})

    (inline-value-form [_ state-sym]
      `(get ~state-sym (quote ~cursor)))))
