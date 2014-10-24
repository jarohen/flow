(ns flow.forms.lenses
  (:require #+clj [flow.compiler :as fc]
            [flow.state :as fs]))

(defmethod fc/compile-el-form :unwrap-lens [[_ lens-sym] opts]
  )

(defmethod fc/compile-value-form :unwrap-lens [[_ lens-sym] opts]
  `(fs/read-lens ~lens-sym))
