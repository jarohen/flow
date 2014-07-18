(ns flow.forms.symbol
  (:require [flow.util :as u]
            [flow.dom :as fd]
            [flow.protocols :as fp]))

(defn symbol->el [quoted-sym]
  (let [!$box (atom nil)]
    (reify fp/Box
      (should-update? [_ updated-vars]
        (u/deps-updated? #{quoted-sym} updated-vars))

      (build [_ state]
        (let [$initial-el (fd/->node (get state quoted-sym))]
          (reset! !$box $initial-el)
          $initial-el))

      (handle-update! [_ old-state new-state updated-vars]
        (let [$new-el (fd/->node (get new-state quoted-sym))]
          (fd/swap-elem! @!$box $new-el)
          (reset! !$box $new-el))))))
