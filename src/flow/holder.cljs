(ns flow.holder
  (:require [flow.dom :as fd]
            [flow.protocols :as fp]
            [flow.diff :refer [vector-diff]]))

(defprotocol ElementHolder
  (append-to! [_ $parent])
  (swap-child! [_ $new-el]))

(defn swap-elem-seqs! [old-els new-els]
  (let [old-el-cache (->> old-els
                          (map (juxt fp/get-flow-id identity))
                          (into {}))

        new-el-cache (->> new-els
                          (map (juxt fp/get-flow-id identity))
                          (into {}))

        diff (vector-diff (map fp/get-flow-id old-els)
                          (map fp/get-flow-id new-els))

        $last-elem (last old-els)
        $next-sibling (.-nextSibling $last-elem)
        $parent (.-parentNode $last-elem)]

    (doseq [[action id] diff]
      (let [$el (get old-el-cache id)]
        (when (= action :moved-out)
          (fd/remove-child! $parent $el))))

    (reduce (fn [$next-sibling [action id]]
              (let [$el (get new-el-cache id)]
                (case action
                  :kept $el

                  :moved-in (do
                              (fd/insert-child-before! $parent $el $next-sibling)
                              $el)

                  :moved-out $next-sibling)))
            
            $next-sibling

            (reverse diff))
    
    new-els))

(defn new-multi-holder [$els]
  (let [!current-els (atom $els)]
    (reify ElementHolder
      (append-to! [_ $parent]
        (fd/append-child! $parent @!current-els))
      
      (swap-child! [_ $new-el]
        (let [$new-el (fd/->node $new-el)]
          (if-not (seq? $new-el)
            (let [$first-el (first @!current-els)]
              (fd/insert-child-before! (.-parentNode $first-el)
                                       $new-el
                                       $first-el)
              (doseq [$el @!current-els]
                (fd/remove! $el)))

            (reset! !current-els (swap-elem-seqs! @!current-els $new-el))))))))

(defn upgrade-to-holder [$old-el $new-els]
  (let [$parent (.-parentNode $old-el)]
    (doseq [$new-el $new-els]
      (fd/insert-child-before! $parent $new-el $old-el)))
  
  (fd/remove! $old-el)
  (new-multi-holder $new-els))

(extend-protocol ElementHolder
  js/Node
  (append-to! [$el $parent]
    (fd/append-child! $parent $el))

  (swap-child! [$el $new-elem]
    (let [$new-elem (fd/->node $new-elem)]
      (if (seq? $new-elem)
        (upgrade-to-holder $el $new-elem)

        (let [$parent (.-parentNode $el)]
          (.replaceChild $parent $new-elem $el)
          $new-elem)))))

(defn new-element-holder [$el-or-$els]
  (let [$el-or-$els (fd/->node $el-or-$els)]
    (if (seq? $el-or-$els)
      (new-multi-holder $el-or-$els)
      $el-or-$els)))
