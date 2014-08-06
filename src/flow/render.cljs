(ns flow.render
  (:require [flow.holder :as fh]
            [flow.state :as fs]))

(defn with-animation-frame-if-possible [f]
  (if (exists? js/requestAnimationFrame)
    (js/requestAnimationFrame f)
    (js/setTimeout f 0)))

(defn new-batch-state [initial-system]
  (atom (assoc initial-system
          :updated-vars #{})))

(defn schedule-frame! [!batch-state]
  (swap! !batch-state
         (fn [{:keys [update-fn updated-vars] :as system}]
           (if (and update-fn
                    (seq updated-vars))
             (do
               (with-animation-frame-if-possible
                 (fn []
                   (swap! !batch-state

                          ;; we don't take update-fn or updated-vars
                          ;; from here as they've already been cleared
                          (fn [{:keys [state $el] :as system}]
                            (let [[$new-el new-update-fn] (binding [fs/*state* (fs/with-updated-vars state
                                                                                 updated-vars)]
                                                            (update-fn))]

                              (assoc system
                                :$el (if-not (= $el $new-el)
                                       (fh/swap-child! $el $new-el)
                                       $el)
                                :update-fn new-update-fn))))
                   
                   (js/setTimeout #(schedule-frame! !batch-state) 0)))

               (assoc system
                 :updated-vars #{}
                 :update-fn nil))

             system))))

(defn effect-updates! [!batch-state dep-sym new-value]
  (swap! !batch-state
         (fn [{:keys [state $el update-fn] :as system}]
           (-> system
               (assoc-in [:state dep-sym] new-value)
               (update-in [:updated-vars] conj dep-sym))))
  
  (schedule-frame! !batch-state))

(defn build-el [{:keys [deps build-fn]}]
  (let [initial-state (->> (for [{:keys [dep-sym dep]} deps]
                             [dep-sym (deref dep)])
                           (into {}))

        [$initial-el initial-update-fn] (binding [fs/*state* initial-state]
                                          (build-fn))
        
        !state (atom initial-state)
        !$el (atom $initial-el)
        !update-fn (atom initial-update-fn)

        !batch-state (new-batch-state {:state initial-state
                                       :$el $initial-el
                                       :update-fn initial-update-fn})]

    (doseq [{:keys [dep-sym dep]} deps]
      (add-watch dep (str (gensym "watch"))
                 (fn flow-dep-watcher [_ _ old-value new-value]
                   (when (not= old-value new-value)
                     (effect-updates! !batch-state dep-sym new-value)))))
    
    $initial-el))
