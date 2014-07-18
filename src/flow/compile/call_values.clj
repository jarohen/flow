(ns flow.compile.call-values
  (:require [flow.compile :refer [compile-form]]))

(defmulti compile-call-form
  (fn [{:keys [call-type]} opts]
    call-type))

(defmethod compile-call-form :if [{:keys [test then else]} opts]
  (let [[compiled-test compiled-then compiled-else] (map #(compile-form % opts) [test then else])]
    {:deps (set (mapcat :deps [compiled-test compiled-then compiled-else]))
     :inline-value `(if ~(:inline-value compiled-test)
                      ~(:inline-value compiled-then)
                      ~(:inline-value compiled-else))}))

(defmethod compile-call-form :do [{:keys [side-effects return]} opts]
  (let [{:keys [deps inline-value]} (compile-form return opts)]
    {:deps deps
     :inline-value `(do
                      ~@side-effects
                      ~inline-value)}))

(defn with-compiled-values [{:keys [bind value path] :as binding} opts]
  (assoc binding
    :compiled-value (compile-form value opts)))

(defmethod compile-call-form :unwrap-cursor [{:keys [cursor path]}
                                              {:keys [dynamic-syms state]}]
  {:deps #{cursor}
   :inline-value `(get ~state (quote ~cursor))})

(defmethod compile-call-form :fn-call [{:keys [args]} opts]
  (let [compiled-args (map #(compile-form % opts) args)
        deps (set (mapcat :deps compiled-args))
        value (map :inline-value compiled-args)]
    {:deps deps
     :inline-value value}))

(defmethod compile-call-form :fn-decl [{:keys [fn-name arities]} opts]
  {:inline-value `(fn ~@(when fn-name [fn-name])
                    ~@(for [{:keys [args body]} arities]
                        `(~args ~(:inline-value (compile-form body opts)))))})

(defmethod compile-form :call [call opts]
  (compile-call-form call opts))
