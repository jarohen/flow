(ns flow.forms.if-test
  (:require [flow.core :as f]
            [flow.render-harness :as fr]
            [flow.dom.elements :as fde]
            [clojure.test :refer :all]))

(deftest if-flips-attr
  (fr/with-render-harness
    (let [!parent (atom {})
          !entries (atom [])]
      (f/root !parent
        (f/el
          [:div {:data-empty (if (empty? (<< !entries))
                               "empty"
                               "full")}]))

      (fr/render-frame!)
      
      (is (= (:attrs (first (:children (fr/el-snapshot !parent))))
             {:data-empty "empty"}))

      (reset! !entries [:not :empty])

      (fr/render-frame!)

      (is (= (:attrs (first (:children (fr/el-snapshot !parent))))
             {:data-empty "full"})))))

(deftest if-flips-message
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
             (if (empty? (<< !entries))
               [:p "No elements in the atom."]
               [:h3 "Some elements!"])]))

        (fr/render-frame!)
        
        (is (= {:tag "p"
                :children [{:children [],
                            :text "No elements in the atom."}]}
               
               (div-contents)))

        (reset! !entries [:not :empty])

        (fr/render-frame!)

        (is (= {:tag "h3"
                :children [{:children [],
                            :text "Some elements!"}]}
               
               (div-contents)))

        (reset! !entries [:still :not :empty])

        (with-redefs [fde/new-element (constantly (atom {:tag "don't create new elements!"}))]
          (fr/render-frame!))
        
        (is (= {:tag "h3"
                :children [{:children [],
                            :text "Some elements!"}]}
               
               (div-contents)))))))

