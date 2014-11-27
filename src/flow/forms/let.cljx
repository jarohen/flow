(ns flow.forms.let
  (:require #+clj [flow.compiler :as fc]
            #+clj [flow.forms.bindings :as fb]
            [flow.state :as fs]))

(defn build-let [compiled-bindings build-body]
  (fn []
    (letfn [(update-let! [body]
              (binding [fs/*state* (reduce (fn [state {:keys [value-fn destructure-fn]}]
                                             (binding [fs/*state* state]
                                               (merge state
                                                      (destructure-fn (value-fn)))))
                                           fs/*state*
                                           compiled-bindings)]
                
                (let [[$el update-body!] ((or body (build-body)))]
                  [$el #(update-let! update-body!)])))]
      
      (update-let! nil))))

#+clj
(defmethod fc/compile-el-form :let [[_ bindings & body] opts]
  (let [{:keys [compiled-bindings opts]} (fb/compile-el-bindings bindings opts)]
    `(build-let ~(vec compiled-bindings)
                (fn []
                  ~(fc/compile-el-form `(do ~@body) opts)))))

#+clj
(defmethod fc/compile-value-form :let [[_ bindings & body] opts]
  (let [{:keys [compiled-bindings opts]} (fb/compile-value-bindings bindings opts)]
    `(let [~@(apply concat compiled-bindings)]
       ~(fc/compile-value-form `(do ~@body) opts))))



