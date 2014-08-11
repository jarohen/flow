(ns flow.compile.do
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.compile :refer [compile-identity compile-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-identity :do [{:keys [side-effects return]} {:keys [path] :as opts}]
  (let [do-el (u/path->sym path "do")
        compiled-side-effects (map #(compile-value %1 (u/with-more-path opts ["do" (str %2)]))
                                   side-effects (range))
        compiled-return (compile-identity return (cond-> opts
                                                   side-effects (u/with-more-path ["do-return"])))

        do-sym (u/path->sym path "do")]

    (if (empty? side-effects)
      compiled-return

      (reify fp/CompiledIdentity
        (hard-deps [_] (set (concat (mapcat fp/value-deps compiled-side-effects)
                                    (fp/hard-deps compiled-return))))

        (soft-deps [_] (fp/soft-deps compiled-return))

        (declarations [_]
          (concat (fp/declarations compiled-return)
                  `[(defn ~do-sym []
                      (letfn [(update-do# [update-fn#]
                                (fn []
                                  ~@(map fp/inline-value-form compiled-side-effects)

                                  (let [[new-value# new-update-fn#] (update-fn#)]
                                    [new-value# (update-do# new-update-fn#)])))]

                        ~@(map fp/inline-value-form compiled-side-effects)

                        (let [[initial-value# initial-update-fn#] ~(fp/build-form compiled-return)]
                          [initial-value# (update-do# initial-update-fn#)])))]))
      
        (build-form [_]
          `(~do-sym))))))

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
                       (fp/value-deps compiled-return))))
        
        (inline-value-form [_]
          `(do
             ~@(map fp/inline-value-form compiled-side-effects)
             ~(fp/inline-value-form compiled-return)))))))
