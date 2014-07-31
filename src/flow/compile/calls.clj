(ns flow.compile.calls
  (:require [flow.compile :refer [compile-el compile-value]]))

(defmulti compile-call-el
  (fn [call opts]
    (:call-type call)))

(defmulti compile-call-value
  (fn [call opts]
    (:call-type call)))

(require 'flow.compile.fn-decl)
(require 'flow.compile.fn-call)
(require 'flow.compile.do)
(require 'flow.compile.unwrap-cursor)
(require 'flow.compile.if)
(require 'flow.compile.case)
(require 'flow.compile.let)
(require 'flow.compile.for)

(defmethod compile-el :call [call opts]
  (compile-call-el call opts))

(defmethod compile-value :call [call opts]
  (compile-call-value call opts))
