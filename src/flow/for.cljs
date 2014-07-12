(ns flow.for
  (:require [flow.util :as u]
            [flow.dom :as fd]
            [flow.protocols :as fp]
            [flow.diff :refer [vector-diff]]))

(defn for->el [quoted-deps for-values make-body-el]
  (let [!last-els (atom nil)
        $placeholder (fd/null-elem)]

    (letfn [(with-el [{:keys [state $el], value-keys :keys, :as value} el-cache]
              (if-let [{:keys [$!body $el]} (get el-cache value-keys)]
                (assoc value
                  :$!body $!body
                  :$el $el)

                (let [$!body (make-body-el)]
                  (assoc value
                    :$!body $!body
                    :$el (fp/build-element $!body state)
                    :new? true))))]
      
      (reify fp/DynamicElement
        (should-update? [_ updated-vars]
          (u/deps-updated? quoted-deps updated-vars))

        (build-element [_ state]
          (let [initial-els (->> (for-values state)
                                 (map #(with-el % {}))
                                 (map #(dissoc % :new?)))]

            (reset! !last-els initial-els)

            (or (seq (map :$el initial-els))
                $placeholder)))

        (handle-update! [_ old-state new-state updated-vars]
          (let [old-els @!last-els
                old-el-cache (->> old-els
                                  (map (juxt :keys identity))
                                  (into {}))

                new-els (->> (for-values new-state)
                             (map #(with-el % old-el-cache)))
                
                new-el-cache (->> new-els
                                  (map (juxt :keys identity))
                                  (into {}))

                {:keys [diff]} (vector-diff (map :keys old-els)
                                            (map :keys new-els))

                $last-elem (:$el (last old-els))
                $parent (.-parentNode (or $last-elem $placeholder))]

            (when (and (empty? new-els)
                       (not-empty old-els))
              (fd/insert-child-before! $parent $placeholder $last-elem))
            
            (doseq [[action id] diff]
              (let [{:keys [$!body $el value], old-el-state :state} (get old-el-cache id)]
                (when (= action :moved-out)
                  (fd/remove-child! $parent $el))))

            (loop [[[action id] & more-diff] (reverse diff)
                   $next-sibling (if $last-elem
                                   (.-nextSibling $last-elem)
                                   $placeholder)]

              (when action
                (case action
                  :kept
                  (let [{:keys [$!body $el value], old-el-state :state} (get old-el-cache id)
                        {new-el-state :state} (get new-el-cache id)

                        ;; TODO 
                        updated-vars (keys new-el-state)]

                    (when (fp/should-update? $!body updated-vars)
                      (fp/handle-update! $!body old-el-state new-el-state updated-vars))

                    (recur more-diff $el))

                  :moved-in
                  (let [{:keys [$!body $el value new?], new-el-state :state} (get new-el-cache id)]
                    (when-not new?
                      (let [updated-vars (keys new-el-state)
                            {old-el-state :state} (get old-el-cache id)]
                        (when (fp/should-update? $!body updated-vars)
                          (fp/handle-update! $!body old-el-state new-el-state updated-vars))))

                    (fd/insert-child-before! $parent $el $next-sibling)
                    
                    (recur more-diff $el))

                  (recur more-diff $next-sibling))))
            
            (when (and (empty? old-els)
                       (not-empty new-els))
              (fd/remove-child! $parent $placeholder))

            (reset! !last-els (->> new-els
                                   (map #(dissoc % :new?))))))))))

