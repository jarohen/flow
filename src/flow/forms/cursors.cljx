(ns flow.forms.cursors
  (:require #+clj [flow.compiler :as fc]
            [flow.deps :as fd]
            [flow.cursors :as fcu]))

(defn build-watch [build-cursor]
  (fn []
    (letfn [(update-cursor! []
              [(fd/read-dep (build-cursor)) update-cursor!])]
      (update-cursor!))))

#+clj
(defmethod fc/compile-el-form :watch [[_ cursor-sym] opts]
  `(build-watch (fn []
                   ~(fc/compile-value-form cursor-sym opts))))

#+clj
(defmethod fc/compile-value-form :watch [[_ cursor-sym] opts]
  `(fd/read-dep ~(fc/compile-value-form cursor-sym opts)))

#+clj
(defmethod fc/compile-value-form :wrap [[_ cursor-sym extra-path] opts]
  `(fcu/->atom ~(fc/compile-value-form cursor-sym opts) ~extra-path))
