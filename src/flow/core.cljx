(ns flow.core
  (:require [flow.el :as fel]
            #+clj [flow.compiler :as fc]
            ))

(defn root [$container el]
  (fel/root $container el))

#+clj
(defmacro el [el]
  `(fel/render-el ~(fc/compile-el el)))

(comment
  (require '[flow.render :as fr])

  (fr/foo-with-render-queue
   (let [!test (atom "4em")
         !parent (atom {})]
     (root !parent
       (el
         [:div
          [:span {:flow.core/style {:height (<< !test)}}]]))

     (fr/foo-render-frame!)
     
     !parent)))
