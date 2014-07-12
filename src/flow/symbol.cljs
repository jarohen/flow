(ns flow.symbol
  (:require [flow.util :as u]
            [flow.dom :as fd]
            [flow.protocols :as fp]))

(defn symbol->el [quoted-sym]
  (let [!$placeholder (atom nil)]
    (reify fp/DynamicElement
      (should-update? [_ updated-vars]
        (u/deps-updated? #{quoted-sym} updated-vars))

      (build-element [_ state]
        (let [$initial-el (fd/->node (get state quoted-sym))]
          (reset! !$placeholder $initial-el)
          $initial-el))

      (handle-update! [_ old-state new-state updated-vars]
        (let [$new-el (fd/->node (get new-state quoted-sym))]
          (fd/swap-elem! @!$placeholder $new-el)
          (reset! !$placeholder $new-el))))))
