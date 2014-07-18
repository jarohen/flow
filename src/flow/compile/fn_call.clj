(ns flow.compile.fn-call
  (:require [flow.compile :refer [compile-form]]
            [flow.compile.calls :refer [compile-call-form]]
            [flow.util :as u]
            [flow.protocols :as fp]))

(defmethod compile-call-form :fn-call [{:keys [path args]} opts]
  )

(defmethod compile-call-form :fn-call [{:keys [path args]} opts]
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

(defmethod compile-call-form :fn-call [{:keys [path args]} {:keys [state] :as opts}]
  (let [compiled-args (map #(compile-form % opts) args)
        deps (set (mapcat :deps compiled-args))
        wrapped-deps (set (mapcat :wrapped-deps compiled-args))
        inline-value (map :inline-value compiled-args)
        fn-sym (symbol path)]
    {:deps deps
     :wrapped-deps wrapped-deps
     :declarations (concat (mapcat :declarations compiled-args)
                           [`(defn ~fn-sym []
                               ;; TODO!
                               (reify fp/DynamicValue
                                 (~'initial-value [_# ~state]
                                   
                                   )))])

     :dynamic-value (when (not-empty wrapped-deps)
                      `(~fn-sym))
     
     :decorated-value (reify fp/DynamicValueForm
                        (initial-value-form [_ value-sym state-sym]
                          `(fp/initial-value ~value-sym ~state-sym))
                        (updated-value-form [_ value-sym old-state-sym new-state-sym updated-vars-sym]
                          `(fp/updated-value ~value-sym ~old-state-sym ~new-state-sym ~updated-vars-sym)))}))
