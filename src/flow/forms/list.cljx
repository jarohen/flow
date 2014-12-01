(ns flow.forms.list
  (:require #+clj [flow.compiler :as fc]))

(defn build-list [elem-builders]
  (fn []
    (letfn [(update-list! [elem-updaters]
              (let [updated-elems (map #(apply % [])
                                       (or elem-updaters
                                           (map #(apply % []) elem-builders)))]
                [(map first updated-elems) #(update-list! (map second updated-elems))]))]

      (update-list! nil))))

#+clj
(defmethod fc/compile-el-form :list [[_ & elems] opts]
  `(build-list [~@(map (fn [elem]
                         `(fn []
                            ~(fc/compile-el-form elem opts)))
                       elems)]))

#+clj
(defmethod fc/compile-value-form :list [[_ & elems] opts]
  `(list ~@(map #(fc/compile-value-form % opts) elems)))


