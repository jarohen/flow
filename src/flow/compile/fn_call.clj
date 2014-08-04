(ns flow.compile.fn-call
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.compile :refer [compile-identity compile-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

(defmethod compile-call-value :fn-call [{:keys [args]} opts]
  (let [compiled-args (map #(compile-value % opts) args)]

    (reify fp/CompiledValue
      (value-deps [_] (set (mapcat fp/value-deps compiled-args)))
        
      (inline-value-form [_]
        `(~@(map fp/inline-value-form compiled-args))))))

(defmethod compile-call-identity :fn-call [{:keys [args] :as call} {:keys [path] :as opts}]
  (let [compiled-args (map #(compile-identity %1 (u/with-more-path opts ["call" "arg" (str %2)]))
                           args
                           (range))]

    (if (empty? (set (mapcat fp/soft-deps compiled-args)))
      (u/value->identity (compile-call-value call opts))

      (reify fp/CompiledIdentity
        (hard-deps [_] (set (mapcat fp/hard-deps compiled-args)))
        (soft-deps [_] (set (mapcat fp/soft-deps compiled-args)))

        (declarations [_]
          (concat (mapcat fp/declarations compiled-args)))
        
        (build-form [_]
          `(flow.forms.fn-call/build-call [~@(for [compiled-arg compiled-args]
                                               `(fn []
                                                  ~(fp/build-form compiled-arg)))]))))))
