(ns flow.forms.for
  (:require #+clj [flow.compiler :as fc]
            #+clj [flow.forms.bindings :as fb]
            [flow.dom.elements :as fde]
            [flow.state :as fs]))

(defn for-values [compiled-bindings]
  (reduce (fn [acc {:keys [value-fn destructure-fn]}]
            (->> (for [{:keys [values state]} acc]
                   (binding [fs/*state* state]
                     (for [value (value-fn)]
                       {:state (merge state
                                      (destructure-fn value))
                        :values (conj values value)})))
                 (apply concat)))
          
          [{:state fs/*state*
            :values []}]
          
          compiled-bindings))

(defn build-for [compiled-bindings build-body]
  (fn []
    (letfn [(update-for! [body-cache]
              (let [for-bodies (for [{:keys [state values]} (for-values compiled-bindings)]
                                 (binding [fs/*state* state]
                                   (let [[$el update-body!] ((or (get body-cache values) (build-body)))]
                                     {:$el $el
                                      :update! update-body!
                                      :values values})))]
                [(or (seq (map :$el for-bodies))
                     (fde/null-elem))
                 #(update-for! (->> for-bodies
                                    (map (juxt :values :update!))
                                    (into {})))]))]
      
      (update-for! {}))))

#+clj
(defmethod fc/compile-el-form :for [[_ bindings body] opts]
  (let [{:keys [compiled-bindings opts]} (fb/compile-el-bindings bindings opts)]
    `(build-for ~(vec compiled-bindings)
                (fn []
                  ~(fc/compile-el-form body opts)))))

#+clj
(defmethod fc/compile-value-form :for [[_ bindings body] opts]
  (let [{:keys [compiled-bindings opts]} (fb/compile-value-bindings bindings opts)]
    `(for [~@(apply concat compiled-bindings)]
       ~(fc/compile-value-form body opts))))

(comment
  (let [!numbers (atom [1 4 5])]
    (binding [fs/*state* {'!numbers !numbers}]
      (let [[els update-for!] ((eval (fc/compile-el-form '(for [x (range 4)
                                                                y (<< !numbers)]
                                                            [:span (+ x y)])
                                                         {:bound-syms #{'!numbers}})))]
        (swap! !numbers rest)
        (let [[new-els update-for!] (update-for!)]
          [els new-els]
          (identical? (second els) (first new-els)))))))
