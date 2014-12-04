(ns flow.forms.if
  (:require #+clj [flow.compiler :as fc]))

(defn build-if [test-fn build-then build-else]
  (fn []
    (letfn [(build-branch [test-value]
              (if test-value
                (build-then)
                (build-else)))

            (update-if [old-test-value update-current-branch!]
              (let [new-test-value (boolean (test-fn))
                    
                    new-branch (if (and update-current-branch!
                                        (= old-test-value new-test-value))
                                 update-current-branch!
                                 (build-branch new-test-value))
                     
                    [$branch-el update-branch!] (new-branch)]
                
                [$branch-el #(update-if new-test-value update-branch!)]))]
      
      (update-if nil nil))))

#+clj
(defmethod fc/compile-el-form :if [[_ test then else] opts]
  `(build-if (fn [] ~(fc/compile-value-form test opts))
             (fn [] ~(fc/compile-el-form then opts))
             (fn [] ~(fc/compile-el-form else opts))))

#+clj
(defmethod fc/compile-value-form :if [[_ test then else] opts]
  `(if ~(fc/compile-value-form test opts)
     ~(fc/compile-value-form then opts)
     ~(fc/compile-value-form else opts)))
