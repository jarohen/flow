(ns flow.forms.do-test
  (:require [flow.core :as f]
            [flow.render-harness :as fr]
            [clojure.test :refer :all]))

(deftest do-runs-side-effects
  (fr/with-render-harness
    (let [!parent (atom {})
          !msg (atom "Hello!")
          !update-count (atom 0)]
      (f/root !parent
        (f/el
          [:div
           (do
             (swap! !update-count inc)
             (<< !msg))]))

      (fr/render-frame!)
      
      (is (= (:text (first (:children (first (:children (fr/el-snapshot !parent))))))
             "Hello!"))

      (is (= 1 @!update-count))

      (reset! !msg "Hi!")

      (fr/render-frame!)

      (is (= (:text (first (:children (first (:children (fr/el-snapshot !parent))))))
             "Hi!"))
      (is (= 2 @!update-count)))))

