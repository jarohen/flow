(ns flow.compile.do
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.compile :refer [compile-identity compile-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

#_(defmethod compile-call-identity :do [{:keys [side-effects return]} {:keys [path] :as opts}]
    (let [do-el (u/path->sym path "do")
          compiled-side-effects (map #(compile-value %1 (u/with-more-path opts ["do" (str %2)]))
                                     side-effects (range))
          compiled-return (compile-identity return (cond-> opts
                                                     side-effects (u/with-more-path ["do-return"])))]

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

(defmethod compile-call-value :do [{:keys [side-effects return]} {:keys [path] :as opts}]
  (let [do-el (u/path->sym path)
        compiled-side-effects (map #(compile-value %1 (u/with-more-path opts ["do" (str %2)]))
                                   side-effects (range))
        compiled-return (compile-value return (cond-> opts
                                                side-effects (u/with-more-path ["do-return"])))]

    (if (empty? side-effects)
      compiled-return

      (reify fp/CompiledValue
        (value-deps [_]
          (set (concat (mapcat fp/value-deps compiled-side-effects)
                       (fp/value-deps return))))
        
        (inline-value-form [_]
          `(do
             ~@(map fp/inline-value-form compiled-side-effects)
             ~(fp/inline-value-form return)))))))
