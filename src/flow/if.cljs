(ns flow.if
  (:require [flow.protocols :as fp]
            [flow.dom :as fd]
            [flow.util :as u]))

(defn if->el [quoted-deps ?!test $!then $!else]

  (let [!$placeholder (atom nil)
        !last-test-value (atom nil)]

    (reify fp/DynamicElement
      (should-update? [_ updated-vars]
        (u/deps-updated? quoted-deps updated-vars))

      (build-element [_ state]
        (let [initial-test-value (fp/current-value ?!test state)
              $initial-el (if (fp/current-value ?!test state)
                            (fp/build-element $!then state)
                            (fp/build-element $!else state))]
          (reset! !last-test-value initial-test-value)
          (reset! !$placeholder $initial-el)
          $initial-el))

      (handle-update! [_ old-state new-state updated-vars]
        (letfn [(update-branch [test-value]
                  (if test-value
                    (when (fp/should-update? $!then updated-vars)
                      (fp/handle-update! $!then old-state new-state updated-vars))

                    (when (fp/should-update? $!else updated-vars)
                      (fp/handle-update! $!else old-state new-state updated-vars))))]
        
          (let [old-test-value @!last-test-value
                new-test-value (fp/current-value ?!test new-state)]
            (if (not= (boolean old-test-value) (boolean new-test-value))
              (do
                (reset! !last-test-value new-test-value)

                (let [$new-el (if new-test-value
                                (fp/build-element $!then new-state)
                                (fp/build-element $!else new-state))]
                  (fd/swap-elem! @!$placeholder $new-el)
                  (reset! !$placeholder $new-el)))
              
              (update-branch @!last-test-value))))))))
