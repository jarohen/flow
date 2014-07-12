(ns flow.for
  (:require [flow.util :as u]
            [flow.dom :as fd]
            [flow.protocols :as fp]
            [flow.diff :refer [vector-diff]]))

(defn for->el [quoted-deps for-values make-body-el]
  (let [!last-els (atom nil)
        null-elem (fd/null-elem)]
    
    (reify fp/DynamicElement
      (should-update? [_ updated-vars]
        (u/deps-updated? quoted-deps updated-vars))

      (build-element [_ state]
        (let [initial-els (for [{:keys [state] :as initial-value} (for-values state)]
                            (let [$!body (make-body-el)]
                              (assoc initial-value
                                :$!body $!body
                                :$el (fp/build-element $!body state))))]

          (reset! !last-els initial-els)

          (or (seq (map :$el initial-els))
              null-elem)))

      (handle-update! [_ old-state new-state updated-vars]
        (let [old-els @!last-els
              new-values (for-values new-state)
              {:keys [diff added removed]} (vector-diff (map :keys old-els)
                                                        (map :keys new-values))
              old-el-cache (->> old-els
                                (map (juxt :keys identity))
                                (into {}))

              new-value-cache (->> new-values
                                   (map (juxt :keys identity))
                                   (into {}))

              $first-elem (:$el (first old-els))
              $parent (.-parentNode $first-elem)]

          (prn (first (vals old-el-cache)))
          
          (loop [$current-elem $first-elem
                 [[action id] & more-diff] diff
                 iteration-limit 10]
            (when (pos? iteration-limit)
              (when (= action :kept)
                (let [{:keys [$!body $el value], old-el-state :state} (get old-el-cache id)
                      {new-state :state} (get new-value-cache id)]
                  (when (fp/should-update? $!body (keys new-state))
                    (fp/handle-update! $!body old-el-cache new-state (keys new-state)))))
              
              (when (seq more-diff)
                (recur $current-elem more-diff (dec iteration-limit)))))

          )))))
