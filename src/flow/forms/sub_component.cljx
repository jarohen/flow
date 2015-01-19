(ns flow.forms.sub-component
  (:require #+clj [flow.compiler :as fc]
            [flow.cursors :as fcu]))

(defn unchanged? [[old-value new-value]]
  (or (and (satisfies? fcu/Cursor old-value)
           (satisfies? fcu/Cursor new-value)

           (= (fcu/-!state old-value) (fcu/-!state new-value))
           (= (fcu/-path old-value) (fcu/-path new-value)))
      
      (= old-value new-value)))

(defn build-sub-component [args]
  (fn []
    (letfn [(build-component [arg-values]
              (apply (first arg-values) (rest arg-values)))
          
            (update-sub-component! [old-arg-values update-component!]
              (let [new-arg-values (map #(apply % []) args)
                    arg-pairs (map vector old-arg-values new-arg-values)

                    [$el update-component!] ((if (or (nil? update-component!)
                                                     (not= (count old-arg-values)
                                                           (count new-arg-values))
                                                     (not (every? unchanged? arg-pairs)))
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
