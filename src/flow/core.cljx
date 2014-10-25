(ns flow.core
  (:require [flow.el :as fel]
            #+clj [flow.compiler :as fc]
            ))

(defn root [$container el]
  (fel/root $container el))

#+clj
(defmacro el [el]
  `(fel/render-el ~(fc/compile-el el &env)))

#+clj
(comment
  (require '[flow.render :as fr])

  (defn foo-el-snapshot [!el]
    (-> @!el
        (update-in [:children] #(map foo-el-snapshot %))))

  (fr/foo-with-render-queue
   (let [!test (atom "4em")
         !parent (atom {})
         !class-test (atom "blah")]
     (root !parent
       (el
         [:div
          [:a#the-link.class {:href "testing"
                              ::classes [(<< !class-test)]}
           [:span {:flow.core/style {:height (<< !test)}}
            "testing here!"]]]))

     (fr/foo-render-frame!)

     (reset! !test "5em")
     (reset! !class-test nil)

     (fr/foo-render-frame!)

     (reset! !class-test ["blah" "more"])
     
     (reset! !test "5em")

     (fr/foo-render-frame!)
     
     (foo-el-snapshot !parent))))
