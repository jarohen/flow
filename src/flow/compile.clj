(ns flow.compile)

(defmulti compile-form
  (fn [form opts]
    (:type form)))

#_(require 'flow.compile.nodes)
#_(require 'flow.compile.calls)
#_(require 'flow.compile.simple)
(require 'flow.compile.symbol)
