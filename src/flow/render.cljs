(ns flow.render
  (:require [flow.holder :as fh]
            [flow.state :as fs]))

(defn build-el [{:keys [deps build-fn]}]
  (let [initial-state (->> (for [{:keys [dep-sym dep]} deps]
                             [dep-sym (deref dep)])
                           (into {}))

        [$initial-el initial-update-fn] (binding [fs/*state* initial-state]
                                          (build-fn))
        
        !state (atom initial-state)
        !$el (atom $initial-el)
        !update-fn (atom initial-update-fn)]

    (doseq [{:keys [dep-sym dep]} deps]
      (add-watch dep (str (gensym "watch"))
                 (fn flow-dep-watcher [_ _ old-value new-value]
                   (when (not= old-value new-value)
                     (let [new-state (swap! !state assoc dep-sym new-value)
                           $old-el @!$el
                           [$new-el new-update-fn] (binding [fs/*state* (assoc !state
                                                                          :updated-vars #{dep-sym})]
                                                     (@!update-fn))]

                       (when-not (= $old-el $new-el)
                         (reset! !el (fh/swap-child! $old-el $new-el))))))))
    
    $initial-el))
