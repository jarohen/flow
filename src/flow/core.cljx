(ns flow.core
  (:require [flow.el :as fel]
            #+clj [flow.compiler :as fc]))

(defn root [$container el]
  (fel/root $container el))

#+clj
(defmacro el [el]
  `(fel/render-el ~(fc/compile-el el)))

