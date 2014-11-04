(ns flow.forms.let-test
  (:require [flow.forms.let :refer :all]
            [flow.compiler :as fc]
            [flow.state :as fs]
            [flow.deps :as fd]
            [clojure.test :refer :all]))

(deftest let-with-destructuring
  (let [!test (atom 8)]
    (binding [fs/*state* {'!test !test}
              fd/*ctx* (reify fd/Context
                         (-read-dep [_ dep]
                           (deref dep)))]
      (let [[$el update!] ((eval (fc/compile-el-form '(let [x 4
                                                            test (<< !test)]
                                                        (str "x is " x " and test is " test))
                                                     {:bound-syms #{'!test}})))]
        
        (reset! !test 9)

        (let [[$new-el update!] (update!)]
          
          (is (= $el "x is 4 and test is 8"))
          (is (= $new-el "x is 4 and test is 9")))))))
