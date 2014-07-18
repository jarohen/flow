(ns flow.forms.cursors
  (:require [flow.protocols :as fp]
            [flow.util :as u]
            [flow.dom :as fd]))

(defn cursor->el [quoted-deps quoted-cursor]
  (let [!$el (atom nil)]
    (reify fp/Box
      (should-update? [_ updated-vars]
        (u/deps-updated? quoted-deps updated-vars))

      (build [_ state]
        (let [$new-el (fd/->node (get state quoted-cursor))]
          
          (when-let [$old-el @!$el]
            (fd/swap-elem! $old-el $new-el))
          
          (reset! !$el $new-el)
          $new-el))
      
      (handle-update! [this old-state new-state updated-vars]
        (fp/build this new-state)))))
