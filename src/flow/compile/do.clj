(ns flow.compile.do
  (:require [flow.compile :refer [compile-el]]
            [flow.compile.calls :refer [compile-call-el]]
            [flow.bindings :as b]
            [flow.util :as u]
            [clojure.set :as set]
            [flow.protocols :as fp]))

(defmethod compile-call-el :do [{:keys [path side-effects return]} opts]
  (let [do-el (symbol path)
        compiled-return (compile-el return opts)]

    (assert (empty? side-effects) "I can't handle this yet!")
    
    (if (empty? side-effects)
      compiled-return

      ;; TODO! handle the non-empty side-effects case
      #_{:el `(~do-el)
         :deps deps
         :declarations (concat (:declarations compiled-return)

                               [`(defn ~do-el []
                                   (let [downstream-el# ~(:el compiled-return)]
                                   
                                     (reify fp/DynamicValue
                                       (~'should-update? [~'_ updated-vars#]
                                         (fp/should-update? downstream-el# updated-vars#))

                                       (~'build [~'_ state#]
                                         ~@side-effects
                                         (fp/build downstream-el#))

                                       (~'handle-update! [~'_ new-state# updated-vars#]
                                         (fp/handle-update! downstream-el# new-state# updated-vars#)))))])})))
