(ns flow.forms.do
  (:require #+clj [flow.compiler :as fc]))

(defn build-do [run-side-effects! build-body]
  (fn []
    (letfn [(update-do! [body]
              (run-side-effects!)
              (let [body (or body (build-body))
                    [$el update-body!] (body)]
                [$el (update-do! update-body!)]))]
      (update-do! nil))))

#+clj
(defmethod fc/compile-el-form :do [[_ & body] opts]
  (let [side-effects (butlast body)
        expr (last body)]
    (if (empty? side-effects)
      (fc/compile-el-form expr opts)
      `(build-do
        (fn []
          ~@(map #(fc/compile-value-form % opts) side-effects))
        (fn []
          (fc/compile-el-form expr opts))))))

#+clj
(defmethod fc/compile-value-form :do [[_ & body] opts]
  `(do
     ~@(map #(fc/compile-value-form % opts) body)))
