(ns flow.forms.sub-component
  (:require #+clj [flow.compiler :as fc]
            [flow.lenses :as fl]))

(defn unchanged? [old-value new-value]
  (or (and (satisfies? fl/Lens old-value)
           (satisfies? fl/Lens new-value)
           (= (fl/-!state old-value) (fl/-!state new-value))
           (= (fl/-path old-value) (fl/-path new-value)))
      (identical? old-value new-value)))

(defn build-sub-component [args]
  (fn []
    (letfn [(build-component [arg-values]
              (apply (first arg-values) (rest arg-values)))
          
            (update-sub-component! [old-arg-values update-component!]
              (let [new-arg-values (map #(apply % []) args)
                    arg-pairs (map vector old-arg-values new-arg-values)

                    [$el update-component!] ((if (or (not= (count old-arg-values)
                                                           (count new-arg-values))
                                                     (not (every? #(apply unchanged? %)
                                                                  arg-pairs)))
                                               (build-component new-arg-values)
                                               update-component!))]
              
                [$el #(update-sub-component! new-arg-values update-component!)]))]
    
      (update-sub-component! nil nil))))

#+clj
(defmethod fc/compile-el-form :sub-component [component-args opts]
  `(build-sub-component [~@(map (fn [arg]
                                  `(fn []
                                     ~(fc/compile-value-form arg opts)))
                                component-args)]))

(comment
  (let [component (fn [x y]
                    (letfn [(update! []
                              [(+ x y) update!])]
                      (update!)))
        [$el update!] (build-sub-component [(fn []
                                              component)
                                            (fn [] (rand-int 3))
                                            (fn [] 5)])
        [$new-el update!] (update!)]
    [$el $new-el]))
