(ns flow.core
  (:require [flow.el :as fel]
            [flow.cursors :as fcu]
            [flow.dom.elements :as fde]
            [flow.forms.text]
            [flow.forms.node]
            [flow.forms.primitive]
            [flow.forms.cursors]
            [flow.forms.collections]
            [flow.forms.list]
            [flow.forms.symbols]
            [flow.forms.fn-calls]
            [flow.forms.fn-decls]
            [flow.forms.do]
            [flow.forms.if]
            [flow.forms.case]
            [flow.forms.let]
            [flow.forms.for]
            [flow.forms.sub-component])
  #+clj (:require [flow.compiler :as fc]))

(defn root [$container el]
  (fel/root $container el))

#+clj
(defmacro el [el]
  `(fel/render-el ~(fc/compile-el el &env)))

#+cljs
(defn bind-value! [cursor]
  (fde/bind-value! cursor))

#+cljs
(defn on [$el event listener]
  (fde/add-event-listener! $el event listener))

(defn keyed-by [f coll]
  (when coll
    (fcu/keyed-by f coll)))
