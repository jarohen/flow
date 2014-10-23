(ns flow.compile
  (:require [flow.protocols :as fp]
            [flow.util :as u]))

(defmulti compile-identity
  (fn [form opts]
    (:type form)))

(defmulti compile-value
  (fn [form opts]
    (:type form)))

(defmethod compile-identity :default [form opts]
  (u/value->identity (compile-value form opts)))

(require 'flow.compile.nodes)
(require 'flow.compile.calls)
(require 'flow.compile.simple)
(require 'flow.compile.symbol)
