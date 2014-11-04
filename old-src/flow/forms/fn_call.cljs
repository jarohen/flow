(ns flow.forms.fn-call)

(defn build-call [arg-build-fns]
  (letfn [(update-fn [current-args current-res update-fns]
            (fn []
              (let [new-values (for [update-fn update-fns]
                                 (update-fn))
                    new-args (map first new-values)
                    new-res (if (every? true? (map = current-args new-args))
                              current-res
                              (apply (first new-args) (rest new-args)))]
                [new-res
                 (update-fn new-args new-res (map second new-values))])))]
    
    (let [initial-values (map apply arg-build-fns)
          initial-args (map first initial-values)
          initial-res (apply (first initial-args) (rest initial-args))]
      
      [initial-res (update-fn initial-args initial-res (map second initial-values))])))
