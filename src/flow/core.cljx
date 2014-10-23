(ns flow.core
  (:require [flow.el :as fel]
            #+clj [flow.compiler :as fc]))

#+clj
(defmacro root [$container el]
  `(fel/root ~$container (fn [] ~el)))

#+clj
(defmacro el [el]
  `(fel/render-el ~(fc/compile-el el)))

