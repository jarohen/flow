(ns flow.core
  (:require [flow.el :as fel])
  #+clj (:require [flow.compiler :as fc]
                  [flow.forms.text]
                  [flow.forms.node]
                  [flow.forms.primitive]
                  [flow.forms.lenses]
                  [flow.forms.collections]
                  [flow.forms.symbols]
                  [flow.forms.fn-calls]
                  [flow.forms.fn-decls]
                  [flow.forms.do]
                  [flow.forms.if]
                  [flow.forms.case]
                  [flow.forms.let]
                  [flow.forms.for]
                  [flow.forms.sub-component]))

(defn root [$container el]
  (fel/root $container el))

#+clj
(defmacro el [el]
  `(fel/render-el ~(fc/compile-el el &env)))
