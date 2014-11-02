(ns flow.forms.lenses
  (:require #+clj [flow.compiler :as fc]
            [flow.deps :as fd]
            [flow.lenses :as fl]))

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

#+clj
(defmethod fc/compile-value-form :wrap-lens [[_ lens-sym extra-path] opts]
  `(fl/->atom ~(fc/compile-value-form lens-sym opts) ~extra-path))
