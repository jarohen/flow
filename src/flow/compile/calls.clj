(ns flow.compile.calls
  (:require [flow.compile :refer [compile-identity compile-value]]
            [flow.util :as u]))

(defmulti compile-call-identity
  (fn [call opts]
    (:call-type call)))

(defmulti compile-call-value
  (fn [call opts]
    (:call-type call)))

(defmethod compile-call-identity :default [call opts]
  (u/value->identity (compile-call-value call opts)))

(require 'flow.compile.fn-decl)
(require 'flow.compile.fn-call)
(require 'flow.compile.do)
(require 'flow.compile.unwrap-cursor)
#_(require 'flow.compile.wrap-cursor)
(require 'flow.compile.if)
#_(require 'flow.compile.case)
#_(require 'flow.compile.let)
#_(require 'flow.compile.for)

(defmethod compile-identity :call [call opts]
  (compile-call-identity call opts))

(defmethod compile-value :call [call opts]
  (compile-call-value call opts))
