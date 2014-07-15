(ns flow.compile.unwrap-cursor
  (:require [flow.compile.calls :refer [compile-call compile-call-value]]
            [flow.util :as u]
            [flow.protocols :as fp]))

(defmethod compile-call :unwrap-cursor [{:keys [cursor]} opts]
  (let [deps #{cursor}]
    
    {:el `(flow.forms.cursors/cursor->el ~(u/quote-deps deps) (quote ~cursor))
     :deps #{cursor}}))

(defmethod compile-call-value :unwrap-cursor [{:keys [cursor path]} opts]
  {:deps #{cursor}
   :decorated-value (reify fp/DynamicValueForm
                      (initial-value-form [_ _ state]
                        `(get ~state (quote ~cursor)))
                      (updated-value-form [_ _ _ new-state _]
                        `(get ~new-state (quote ~cursor))))})
