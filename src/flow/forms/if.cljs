(ns flow.forms.if
  (:require [flow.protocols :as fp]
            [flow.dom :as fd]
            [flow.util :as u]))

(defn if->el [quoted-deps test-value $!then $!else]

  (let [!$box (atom nil)
        !last-test-value (atom nil)]

    (reify fp/Box
      (should-update? [_ updated-vars]
        (u/deps-updated? quoted-deps updated-vars))

      (build [_ state]
        (let [initial-test-value (test-value state)
              $initial-el (if initial-test-value
                            (fp/build $!then state)
                            (fp/build $!else state))]
          (reset! !last-test-value initial-test-value)
          (reset! !$box $initial-el)
          $initial-el))

      (handle-update! [_ old-state new-state updated-vars]
        (letfn [(update-branch [test-value]
                  (if test-value
                    (when (fp/should-update? $!then updated-vars)
                      (fp/handle-update! $!then old-state new-state updated-vars))

                    (when (fp/should-update? $!else updated-vars)
                      (fp/handle-update! $!else old-state new-state updated-vars))))]
        
          (let [old-test-value @!last-test-value
                new-test-value (test-value new-state)]
            (if (not= (boolean old-test-value) (boolean new-test-value))
              (do
                (reset! !last-test-value new-test-value)

                (let [$new-el (if new-test-value
                                (fp/build $!then new-state)
                                (fp/build $!else new-state))]
                  (fd/swap-elem! @!$box $new-el)
                  (reset! !$box $new-el)))
              
              (update-branch @!last-test-value))))))))
