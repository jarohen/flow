(ns flow.forms.for
  (:require #+clj [flow.compiler :as fc]
            #+clj [flow.forms.bindings :as fb]
            [flow.dom.elements :as fde]
            [flow.cursors :as fcu]
            [flow.state :as fs]))

(defn value-pk [value]
  (if (fcu/cursor? value)
    [::cursor (fcu/-!state value) (fcu/-path value)]
    value))

(defn for-values [compiled-bindings]
  (reduce (fn [acc {:keys [value-fn destructure-fn]}]
            (->> (for [{:keys [state pks]} acc]
                   (binding [fs/*state* state]
                     (->> (for [value (value-fn)]
                            {:state (merge state
                                           (destructure-fn value))
                             :pks (conj pks (value-pk value))})
                          doall)))
                 (apply concat)))
          
          [{:state fs/*state*
            :pks []}]
          
          compiled-bindings))

(defn build-for [compiled-bindings build-body]
  (fn []
    (letfn [(update-for! [body-cache]
              (let [for-bodies (->> (for [{:keys [state pks]} (for-values compiled-bindings)]
                                      (binding [fs/*state* state]
                                        (let [[$el update-body!] ((or (get body-cache pks) (build-body)))]
                                          {:$el $el
                                           :update! update-body!
                                           :pks pks})))
                                    doall)]
                
                [(or (seq (map :$el for-bodies))
                     (fde/null-elem))
                 #(update-for! (into {} (map (juxt :pks :update!)) for-bodies))]))]
      
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

