(ns flow.compile.wrap-cursor
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-identity :wrap-cursor [{:keys [cursor]} {:keys [path] :as opts}]
  (let [!wrapped-atom (u/path->sym "!" path "value")
        deps #{cursor}]
    (reify fp/CompiledIdentity
      (identity-deps [_] deps)
      (bindings [_]
        `[[~!wrapped-atom (atom nil)]])

      (initial-form [_ state-sym]
        `(let [initial-value# (get ~state-sym (quote ~cursor))]
           (reset! ~!wrapped-atom initial-value#)
           ~!wrapped-atom))

      (updated-form [_ new-state-sym updated-vars-sym]
        (u/with-updated-deps-check deps updated-vars-sym
          `(do
             (reset! ~!wrapped-atom (get ~new-state-sym (quote ~cursor)))
             ~!wrapped-atom)
          
          !wrapped-atom)))))
