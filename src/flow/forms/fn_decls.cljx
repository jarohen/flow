(ns flow.forms.fn-decls
  (:require #+clj [flow.compiler :as fc]
            [flow.state :as fs]
            [clojure.set :as set]))

#+clj
(defmethod fc/compile-value-form :fn-decl [[_ & arities] opts]
  (let [state-sym (gensym "state")]
    `(let [~state-sym fs/*state*]
       (fn*
        ~@(for [[args & body] (if (seq? (first arities))
                                arities
                                [arities])]
            `(~args (binding [fs/*state* ~state-sym]
                      ~(fc/compile-value-form `(do ~@body)
                                              (update-in opts [:bound-syms] set/difference (set args))))))))))

(comment
  (fc/compile-value-form (macroexpand '(fn [{:keys [a]}]
                                         (+ a 4)))
                         {:bound-syms #{}})


  ((macroexpand '(fn [{:keys [a]}]
                   (+ a 4)))))
