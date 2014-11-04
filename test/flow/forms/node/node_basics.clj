(ns flow.forms.node.node-basics
  (:require [flow.core :as f]
            [flow.render-harness :as fr]
            [flow.dom.attributes :as fda]
            [clojure.test :refer :all]))

(deftest static-node
  (fr/with-render-harness
    (let [!parent (atom {})]
      (f/root !parent
        (f/el
          [:a#the-link {:href ""
                        ::f/style {:color "#4ff"}}
           "Link content"]))

      (fr/render-frame!)
      
      (is (= (fr/el-snapshot !parent)
             {:children [{:tag "a"
                          :id "the-link"
                          :style {:color "#4ff"},
                          :attrs {:href ""}
                          :children [{:children [], :text "Link content"}]}]})))))

(deftest updating-style
  (fr/with-render-harness
    (let [!parent (atom {})
          !color (atom "#4ff")]
      (f/root !parent
        (f/el
          [:a {::f/style {:color (<< !color)}}
           "Link content"]))

      (fr/render-frame!)
      
      (is (= (fr/el-snapshot !parent)
             {:children [{:tag "a"
                          :style {:color "#4ff"},
                          :children [{:children [], :text "Link content"}]}]}))

      (reset! !color "#4dd")
      
      (fr/render-frame!)

      (is (= (fr/el-snapshot !parent)
             {:children [{:tag "a"
                          :style {:color "#4dd"},
                          :children [{:children [], :text "Link content"}]}]})))))

(deftest updating-classes
  (fr/with-render-harness
    (let [!parent (atom {})
          !class-test (atom :blah)
          !dummy-attr (atom :something)]

      (f/root !parent
        (f/el
          [:div.test-class {::f/classes [(<< !class-test)]
                            :dummy-attr (<< !dummy-attr)}
           "Hello world!"]))

      (fr/render-frame!)

      (is (= #{:test-class :blah}
             (:classes (first (:children (fr/el-snapshot !parent))))))

      (reset! !class-test [:more-than :one-class])

      (fr/render-frame!)

      (is (= #{:test-class :more-than :one-class}
             (:classes (first (:children (fr/el-snapshot !parent))))))

      (reset! !dummy-attr :something-else)

      (with-redefs [fda/set-classes! (fn [&_]
                                       (throw (Exception. "shouldn't update classes!")))]
        (fr/render-frame!))

      (is (= #{:test-class :more-than :one-class}
             (:classes (first (:children (fr/el-snapshot !parent)))))))))
