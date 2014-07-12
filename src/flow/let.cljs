(ns flow.let
  (:require [flow.util :as u]
            [flow.protocols :as fp]
            [clojure.set :as set]))

(defn let->el [?!value $!body quoted-deps bind-syms bind-values-map]
  (let [!last-value (atom nil)]
                                     
    (reify fp/DynamicElement
      (should-update? [_ updated-vars]
        (u/deps-updated? quoted-deps updated-vars))

      (build-element [_ state]
        (let [initial-value (fp/current-value ?!value state)
              initial-el (fp/build-element $!body (merge state (bind-values-map initial-value)))]
                                           
          (reset! !last-value initial-value)
          initial-el))

      (handle-update! [_ old-state new-state updated-vars]
        (letfn [(bind-updated-vars [old-value new-value]
                  (let [old-map (bind-values-map old-value)
                        new-map (bind-values-map new-value)]
                    (set (filter #(not= (get old-map %)
                                       (get new-map %))
                                 bind-syms))))
                
                (update-body [old-value new-value updated-vars]
                  (when (fp/should-update? $!body updated-vars)
                    (fp/handle-update! $!body
                                       (merge old-state (bind-values-map old-value))
                                       (merge new-state (bind-values-map new-value))
                                       updated-vars)))]
                                           
          (let [old-value @!last-value
                new-value (fp/current-value ?!value new-state)]
            (if (not= old-value new-value)
              (do
                (reset! !last-value new-value)
                (update-body @!last-value new-value (doto (set/union updated-vars (bind-updated-vars old-value new-value)) prn)))
             
              (update-body @!last-value @!last-value updated-vars))))))))
