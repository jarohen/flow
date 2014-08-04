(ns flow.compile)

(defmulti compile-form
  (fn [form opts]
    (:type form)))

(require 'flow.compile.nodes)
(require 'flow.compile.calls)
(require 'flow.compile.simple)
(require 'flow.compile.symbol)
