(ns flow.compile.fn-call
  (:require [flow.compile :refer [compile-form]]
            [flow.compile.calls :refer [compile-call-form]]
            [flow.util :as u]
            [flow.protocols :as fp]))

#_(defmethod compile-call-form :fn-call [{:keys [path args]} opts]
    (let [compiled-args (map #(compile-form % opts) args)
          deps (mapcat :deps compiled-args)
          call-box (symbol path)]
      (assert (or value?
                  (empty? deps))
              "")
      {:deps deps
       :value (when value?
                `(~@(map :inline-value compiled-args)))
     
       :declarations (concat (mapcat :declarations compiled-args)
                             [`(defn ~call-box []
                                 (let [!box# (atom nil)]
                         
                                   (reify fp/Box
                                     (~'should-update? [_# updated-vars#]
                                       (u/deps-updated? ~(u/quote-deps deps) updated-vars#))

                                     (~'build [_# ~state]
                                       (let [initial-box# (fd/->node (~@(map :inline-value compiled-args)))]
                                         (reset! !box# initial-box#)
                                         initial-box#))

                                     (~'handle-update! [_1# _2# ~new-state _4#]
                                       (let [$new-el# (let [~state ~new-state]
                                                        (fd/->node (~@(map :inline-value compiled-args))))]
                                         (fd/swap-elem! @!$el# $new-el#)
                                         (reset! !$el# $new-el#))))))])

       :box (when box?
              `(~call-box))}))


