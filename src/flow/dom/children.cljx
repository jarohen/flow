(ns flow.dom.children
  (:require [flow.dom.elements :as fde]
            [flow.dom.diff :as fdd]
            [flow.dom.scheduler :as fds]))

(defn with-lock [obj f]
  #+clj (locking obj (f))
  #+cljs (f))

(defn new-child-holder! [$parent]
  (atom {:$parent $parent
         :el-ish ::init}))

(defn swap-child-seqs! [$parent old-els new-els]
  (let [$last-elem (last old-els)
        $next-sibling (fde/next-sibling $parent $last-elem)

        diff (doall (fdd/vector-diff old-els new-els))]

    (fds/schedule-dom-change
     (fn []
       (doseq [[action $el] diff]
         (when (= action :moved-out)
           (fde/remove-child! $parent $el)))

       (reduce (fn [$next-sibling [action $new-el]]
                 (case action
                   :kept $new-el
                   :moved-in (do
                               (if $next-sibling
                                 (fde/insert-before! $parent $next-sibling $new-el)
                                 (fde/append-child! $parent $new-el))
                               $new-el)
                   :moved-out $next-sibling))
               
               $next-sibling
               (reverse diff))))))

(defn swap-child-el! [$parent $old-el $new-el]
  (case [(coll? $old-el) (coll? $new-el)]
    [false false] (when-not (identical? $old-el $new-el)
                    (fds/schedule-dom-change
                     (fn []
                       (fde/replace-child! $parent $old-el $new-el))))
    [false true] (fds/schedule-dom-change
                  (fn []
                    (doseq [$el $new-el]
                      (fde/insert-before! $parent $old-el $el))
                    (fde/remove-child! $parent $old-el)))
    [true false] (fds/schedule-dom-change
                  (fn []
                    (fde/insert-before! $parent (first $old-el) $new-el)
                    (doseq [$el $old-el]
                      (fde/remove-child! $parent $el))))
    [true true] (swap-child-seqs! $parent $old-el $new-el)))

(defn replace-child! [!child-holder new-el-ish]
  (with-lock !child-holder
    (fn []
      (let [{$parent :$parent, old-el-ish :el-ish, $old-el :$el} @!child-holder]
        (when-not (= old-el-ish new-el-ish)
          (let [$new-el (fde/->el new-el-ish)]
            (reset! !child-holder {:$parent $parent
                                   :el-ish new-el-ish
                                   :$el $new-el})
            (swap-child-el! $parent $old-el $new-el)))))))
