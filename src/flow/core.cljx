(ns flow.core
  (:require [flow.el :as fel]
            #+clj [flow.compiler :as fc]))

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
   (let [!style-test (atom "4em")
         !parent (atom {})
         !class-test (atom "blah")
         style-sym-test "12em"]
     (root !parent
       (el
         [:div
          [:a#the-link.class {:href "testing"
                              ::classes [(<< !class-test)]}
           [:span {::style {:height (<< !style-test)
                            :width style-sym-test}}
            "testing here!"]]]))

     (fr/foo-render-frame!)

     (reset! !style-test "5em")
     (reset! !class-test nil)

     (fr/foo-render-frame!)

     (reset! !class-test ["blah" "more"])
     
     (reset! !style-test "5em")

     (fr/foo-render-frame!)
     
     (foo-el-snapshot !parent))))
