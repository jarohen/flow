(ns flow.compile)

(defmulti compile-el
  (fn [elem env]
    (:type elem)))

(require 'flow.compile.node)
(require 'flow.compile.simple)

