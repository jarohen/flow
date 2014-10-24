(ns flow.forms.text
  (:require #+clj [flow.compiler :as fc]
            [flow.dom.elements :refer [text-el]]))

(defn text-node [s]
  (fn []
    (let [$el (text-el s)]
      (letfn [(update-text! []
                [$el update-text!])]
        [$el update-text!]))))

#+clj
(defmethod fc/compile-el-form :text [s opts]
  `(text-node ~s))
