(ns flow.compile.fn-call
  (:require [flow.compile :refer [compile-value compile-value-form]]
            [flow.compile.calls :refer [compile-call compile-call-value]]
            [flow.util :as u]
            [flow.protocols :as fp]))

(defmethod compile-call :fn-call [{:keys [path args]} {:keys [state old-state new-state updated-vars] :as opts}]
  (let [compiled-args (map #(compile-value % opts) args)
        deps (mapcat :deps compiled-args)
        call-el (symbol path)]
    {:el `(~call-el)
     :deps deps
     :declarations [`(defn ~call-el []
                       (let [!$el# (atom nil)]
                         
                         (reify fp/DynamicElement
                           (~'should-update? [_# updated-vars#]
                             (u/deps-updated? ~(u/quote-deps deps) updated-vars#))

                           (~'build-element [_# ~state]
                             (let [$initial-el# (fd/->node (~@(map :inline-value compiled-args)))]
                               (reset! !$el# $initial-el#)
                               $initial-el#))

                           (~'handle-update! [_1# _2# ~new-state _4#]
                             (let [$new-el# (let [~state ~new-state]
                                             (fd/->node (~@(map :inline-value compiled-args))))]
                               (fd/swap-elem! @!$el# $new-el#)
                               (reset! !$el# $new-el#))))))]}))

(defmethod compile-call-value :fn-call [{:keys [path args]} {:keys [state] :as opts}]
  (let [compiled-args (map #(compile-value % opts) args)
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
