(ns flow.forms.fn-call-test
  (:require [flow.core :as f]
            [flow.render-harness :as fr]
            [clojure.test :refer :all]))

(deftest fn-call-at-root-test
  (fr/with-render-harness
    (let [!parent (atom {})
          !entries (atom [])]
      (f/root !parent
        (f/el
          (str "Hello" " " "World!")))

      (fr/render-frame!)
      
      (is (= {:children [{:children [],
                          :text "Hello World!"}]}
             
             (fr/el-snapshot !parent))))))

(deftest fn-call-count-test
  (fr/with-render-harness
    (let [!parent (atom {})
          !entries (atom [])]
      (letfn [(div-contents []
                (-> (fr/el-snapshot !parent)
                    :children
                    first
                    :children
                    first))]
        (f/root !parent
          (f/el
            [:div
             [:p (str (count (<< !entries)) " element(s) in the atom.")]]))

        (fr/render-frame!)
                                  
        (is (= {:tag "p"
                :children [{:children [],
                            :text "0 element(s) in the atom."}]}
                                         
               (div-contents)))

        (reset! !entries [:not :empty])

        (fr/render-frame!)

        (is (= {:tag "p"
                :children [{:children [],
                            :text "2 element(s) in the atom."}]}
                                         
               (div-contents)))))))

