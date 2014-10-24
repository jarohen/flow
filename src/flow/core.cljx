(ns flow.core
  (:require [flow.el :as fel]
            #+clj [flow.compiler :as fc]))

(defn root [$container el]
  (fel/root $container el))

#+clj
(defmacro el [el]
  `(fel/render-el ~(fc/compile-el el)))

(comment
  (let [!test (atom "4em")]
    (root (atom {})
      (el
        [:div
         [:span {:flow.core/style {:height (<< !test)}}]]))))
