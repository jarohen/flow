(ns flow.forms.case-test
  (:require [flow.core :as f]
            [flow.render-harness :as fr]
            [flow.dom.elements :as fde]
            [clojure.test :refer :all]))

(deftest case-flips-attr
  (fr/with-render-harness
    (let [!parent (atom {})
          !entries (atom [])]
      (f/root !parent
        (f/el
          [:div {:data-count (case (count (<< !entries))
                               0 "none"
                               1 "one"
                               "more")}]))

      (fr/render-frame!)
      
      (is (= (:attrs (first (:children (fr/el-snapshot !parent))))
             {:data-count "none"}))

      (reset! !entries [:not :empty])

      (fr/render-frame!)

      (is (= (:attrs (first (:children (fr/el-snapshot !parent))))
             {:data-count "more"}))

      (reset! !entries [:just-one])

      (fr/render-frame!)

      (is (= (:attrs (first (:children (fr/el-snapshot !parent))))
             {:data-count "one"})))))

(deftest case-flips-message
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
             (case (count (<< !entries))
               0 [:p "No elements in the atom."]
               1 [:p "One element in the atom."]
               [:p "Lots of elements in the atom."])]))

        (fr/render-frame!)
        
        (is (= {:tag "p"
                :children [{:children [],
                            :text "No elements in the atom."}]}
               
               (div-contents)))

        (reset! !entries [:plenty :of :elements])

        (fr/render-frame!)

        (is (= {:tag "p"
                :children [{:children [],
                            :text "Lots of elements in the atom."}]}
               
               (div-contents)))

        (reset! !entries [:still :plenty :of :elements])

        (with-redefs [fde/new-element (constantly (atom {:tag "don't create new elements!"}))]
          (fr/render-frame!))
        
        (is (= {:tag "p"
                :children [{:children [],
                            :text "Lots of elements in the atom."}]}
               
               (div-contents)))))))

