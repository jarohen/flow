(ns flow.forms.text
  (:require #+clj [flow.compiler :refer [compile-form]]
            [flow.dom.elements :refer [text-el]]))

(defn text-node [s]
  (let [$el (text-el s)]
    (fn text []
      [$el text])))

#+clj
(defmethod compile-form :text [s]
  `(text-node ~s))
