(ns flow.forms.text
  (:require #+clj [flow.compiler :as fc]
            [flow.dom.elements :refer [text-el]]))

(defn text-node [s]
  (let [$el (text-el s)]
    (fn text []
      [$el text])))

#+clj
(defmethod fc/compile-el-form :text [s]
  `(text-node ~s))
