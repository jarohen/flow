(ns flow.compile.unwrap-cursor
  (:require [flow.compile.calls :refer [compile-call-form]]
            [flow.protocols :as fp]))

(defmethod compile-call-form :unwrap-cursor [{:keys [cursor]} opts]
  (reify fp/CompiledForm
    (form-deps [_] #{cursor})

    (bindings [_] nil)

    (initial-value-form [_ state-sym]
      `(get ~state-sym (quote ~cursor)))

    (updated-value-form [_ old-state-sym new-state-sym updated-vars-sym]
      `(get ~new-state-sym (quote ~cursor)))))
