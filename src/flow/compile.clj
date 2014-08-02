(ns flow.compile)

(defmulti compile-identity
  (fn [form opts]
    (:type form)))

(defmulti compile-value
  (fn [form opts]
    (:type form)))

(defmethod compile-identity :default [form opts]
  (compile-value form opts))

(require 'flow.compile.nodes)
(require 'flow.compile.calls)
(require 'flow.compile.simple)
(require 'flow.compile.symbol)
