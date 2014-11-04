(ns flow.forms.if
  (:require [flow.state :as fs]))

(defn build-if [compiled-test compiled-then compiled-else]
  (letfn [(update-if [current-test-value current-branch current-value update-fn]
            (letfn [(update-branch []
                      (if (fs/deps-updated? (:deps current-branch))
                        (let [[new-value new-update-fn] (update-fn)]
                          [new-value (update-if current-test-value
                                                current-branch
                                                new-value
                                                new-update-fn)])

                        [current-value (update-if current-test-value
                                                  current-branch
                                                  current-value
                                                  update-fn)]))]
              (fn []
                (if (fs/deps-updated? (:deps compiled-test))
                  (let [new-test-value ((:value-fn compiled-test))]
                    (if (not= (boolean current-test-value)
                              (boolean new-test-value))
                      (let [new-branch (if new-test-value
                                         compiled-then
                                         compiled-else)
                            [new-value new-update-fn] ((:build-fn new-branch))]
                        [new-value (update-if new-test-value new-branch new-value new-update-fn)])
                    
                      (update-branch)))

                  (update-branch)))))]
    
    (let [test-value ((:value-fn compiled-test))
          initial-branch (if test-value
                           compiled-then
                           compiled-else)
          [initial-value update-fn] ((:build-fn initial-branch))]
      [initial-value (update-if test-value initial-branch initial-value update-fn)])))
