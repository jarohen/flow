(ns flow.compile.unwrap-lens
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.protocols :as fp]))

(alias 'fs (doto 'flow.state create-ns))
(alias 'fl (doto 'flow.lens create-ns))

(defmethod compile-call-value :unwrap-lens [{:keys [lens]} opts]
  (reify fp/CompiledValue
    (value-deps [_] #{lens})

    (inline-value-form [_]
      `(get fs/*state* (quote ~lens)))))
