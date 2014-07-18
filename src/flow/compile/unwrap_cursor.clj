(ns flow.compile.unwrap-cursor
  (:require [flow.compile.calls :refer [compile-call-form]]
            [flow.protocols :as fp]))

(defmethod compile-call-form :unwrap-cursor [{:keys [cursor]} opts]
  (reify fp/CompiledForm
    (form-deps [_] #{cursor})
    
    (form-declarations [_] nil)
    
    (current-value-form [_ state-sym]
      `(get ~state-sym (quote ~cursor)))))
