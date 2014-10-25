(ns flow.forms.lenses
  (:require #+clj [flow.compiler :as fc]
            [flow.state :as fs]))

#+clj
(defmethod fc/compile-el-form :unwrap-lens [[_ lens-sym] opts]
  ;; TODO?!
  )

#+clj
(defmethod fc/compile-value-form :unwrap-lens [[_ lens-sym] opts]
  `(fs/read-lens ~lens-sym))
