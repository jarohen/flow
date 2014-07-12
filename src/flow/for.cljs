(ns flow.for
  (:require [flow.util :as u]
            [flow.dom :as fd]
            [flow.protocols :as fp]))

(defn for->el [quoted-deps for-values $!body]
  (let [!last-els (atom nil)
        null-elem (fd/null-elem)]
    
    (reify fp/DynamicElement
      (should-update? [_ updated-vars]
        (u/deps-updated? quoted-deps updated-vars))

      (build-element [_ state]
        (let [initial-els (for [{:keys [state] :as initial-value} (for-values state)]
                            
                            (assoc initial-value
                              :$el (fp/build-element $!body state)))]
          
          (reset! !last-els initial-els)

          (or (seq (map :$el initial-els))
              null-elem)))

      (handle-update! [_ old-state new-state updated-vars]

        ;; TODO
        #_(letfn [(update-body# [old-value# new-value# updated-vars#]
                    (when (fp/should-update? body# updated-vars#)
                      (fp/handle-update! body#
                                         (merge old-state# (bind-values-map# old-value#))
                                         (merge new-state# (bind-values-map# new-value#))
                                         updated-vars#)))]
            
            (let [old-value# @!last-value#
                  new-value# (fp/current-value value# new-state#)]
              (if (not= old-value# new-value#)
                (do
                  (reset! !last-value# new-value#)
                  (update-body# @!last-value# new-value# (set/union updated-vars# (bind-updated-vars# old-value# new-value#))))
                
                (update-body# @!last-value# @!last-value# updated-vars#))))))))
