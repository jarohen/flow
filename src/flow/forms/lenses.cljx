(ns flow.forms.lenses
  (:require #+clj [flow.compiler :as fc]
            [flow.state :as fs]))

(defn build-unwrap [build-lens]
  (fn []
    (letfn [(update-lens! []
              [(fs/read-lens (build-lens)) update-lens!])]
      (update-lens!))))

#+clj
(defmethod fc/compile-el-form :unwrap-lens [[_ lens-sym] opts]
  `(build-unwrap (fn []
                   ~(fc/compile-value-form lens-sym opts))))

#+clj
(defmethod fc/compile-value-form :unwrap-lens [[_ lens-sym] opts]
  `(fs/read-lens ~(fc/compile-value-form lens-sym opts)))
