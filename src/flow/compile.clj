(ns flow.compile
  (:require [flow.util :as u]))

(alias 'fp (doto 'flow.protocols create-ns))

(defmulti compile-el
  (fn [elem opts]
    (:type elem)))

(defmulti compile-value
  (fn [elem opts]
    (:type elem)))

(require 'flow.compile.nodes)
(require 'flow.compile.calls)

(require 'flow.compile.simple)
(require 'flow.compile.call-values)
