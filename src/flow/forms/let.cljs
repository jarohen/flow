(ns flow.forms.let
  (:require [flow.util :as u]
            [flow.protocols :as fp]
            [clojure.set :as set]))

(defn let->el [quoted-deps $!body let-bindings-state]
  (let [!last-body-state (atom nil)]
                                     
    (reify fp/DynamicValue
      (should-update? [_ updated-vars]
        (u/deps-updated? quoted-deps updated-vars))

      (build [_ state]
        (let [bindings-state (let-bindings-state state)
              body-state (merge state bindings-state)
              $initial-el (fp/build $!body body-state)]
                                           
          (reset! !last-body-state body-state)
          $initial-el))

      (handle-update! [_ old-state new-state updated-vars]
        (let [old-body-state @!last-body-state
              new-body-state (merge new-state (let-bindings-state new-state))
              
              updated-vars (keys new-body-state)]
          
          (reset! !last-body-state new-body-state)

          (when (fp/should-update? $!body updated-vars)
            (fp/handle-update! $!body
                               old-body-state
                               new-body-state
                               updated-vars)))))))
