(ns flow.compile.calls
  (:require [flow.compile :refer [compile-form]]
            [flow.bindings :as b]
            [flow.util :as u]
            [clojure.set :as set]
            [flow.protocols :as fp]))

(defmulti compile-call-form
  (fn [call opts]
    (:call-type call)))

(require 'flow.compile.fn-decl)
(require 'flow.compile.fn-call)
(require 'flow.compile.do)
(require 'flow.compile.unwrap-cursor)
(require 'flow.compile.if)
(require 'flow.compile.let)
(require 'flow.compile.for)

(defmethod compile-form :call [call opts]
  (compile-call-form call opts))
