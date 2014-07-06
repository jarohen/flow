(ns flow.compile.call-values
  (:require [flow.compile :refer [compile-value-form]]))

(defmulti compile-call-value
  (fn [{:keys [call-type]} opts]
    call-type))

(defmethod compile-call-value :if [{:keys [test then else]} opts]
  (let [[compiled-test compiled-then compiled-else] (map #(compile-value-form % opts) [test then else])]
    {:deps (set (mapcat :deps [compiled-test compiled-then compiled-else]))
     :inline-value `(if ~(:inline-value compiled-test)
                      ~(:inline-value compiled-then)
                      ~(:inline-value compiled-else))}))

(defmethod compile-call-value :do [{:keys [side-effects return]} opts]
  (let [{:keys [deps inline-value]} (compile-value-form return opts)]
    {:deps deps
     :inline-value `(do
                      ~@side-effects
                      ~inline-value)}))

(defmethod compile-call-value :unwrap-cursor [{:keys [cursor path]}
                                              {:keys [dynamic-syms state]}]
  {:deps #{cursor}
   :inline-value `(get ~state (quote ~cursor))})

(defmethod compile-call-value :fn-call [{:keys [args]} opts]
  (let [compiled-args (map #(compile-value-form % opts) args)
        deps (set (mapcat :deps compiled-args))
        value (map :inline-value compiled-args)]
    {:deps deps
     :inline-value value}))

(defmethod compile-call-value :fn-decl [{:keys [fn-decl]} opts]
  {:inline-value fn-decl})

(defmethod compile-value-form :call [call opts]
  (compile-call-value call opts))

