(ns flow.forms.lenses
  (:require #+clj [flow.compiler :as fc]
            [flow.deps :as fd]
            [flow.lenses.common :as flc]))

(defn build-unwrap [build-lens]
  (fn []
    (letfn [(update-lens! []
              [(fd/read-dep (build-lens)) update-lens!])]
      (update-lens!))))

#+clj
(defmethod fc/compile-el-form :unwrap-lens [[_ lens-sym] opts]
  `(build-unwrap (fn []
                   ~(fc/compile-value-form lens-sym opts))))

#+clj
(defmethod fc/compile-value-form :unwrap-lens [[_ lens-sym] opts]
  `(fd/read-dep ~(fc/compile-value-form lens-sym opts)))
