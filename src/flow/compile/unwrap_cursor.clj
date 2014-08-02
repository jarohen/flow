(ns flow.compile.unwrap-cursor
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.protocols :as fp]))

(alias 'fs (doto 'flow.state create-ns))

(defmethod compile-call-value :unwrap-cursor [{:keys [cursor]} opts]
  (reify fp/CompiledValue
    (value-deps [_] #{cursor})

    (inline-value-form [_]
      `(get fs/*state* (quote ~cursor)))))
