(ns flow.compile.call-values
  (:require [flow.compile :refer [compile-value]]))

(defmulti compile-call-value
  (fn [{:keys [call-type]} opts]
    call-type))

(defmethod compile-call-value :if [{:keys [test then else]} opts]
  (let [[compiled-test compiled-then compiled-else] (map #(compile-value % opts) [test then else])]
    {:deps (set (mapcat :deps [compiled-test compiled-then compiled-else]))
     :inline-value `(if ~(:inline-value compiled-test)
                      ~(:inline-value compiled-then)
                      ~(:inline-value compiled-else))}))

(defmethod compile-call-value :do [{:keys [side-effects return]} opts]
  (let [{:keys [deps inline-value]} (compile-value return opts)]
    {:deps deps
     :inline-value `(do
                      ~@side-effects
                      ~inline-value)}))

(defn with-compiled-values [{:keys [bind value path] :as binding} opts]
  (assoc binding
    :compiled-value (compile-value value opts)))

(defmethod compile-call-value :unwrap-cursor [{:keys [cursor path]}
                                              {:keys [dynamic-syms state]}]
  {:deps #{cursor}
   :inline-value `(get ~state (quote ~cursor))})

(defmethod compile-call-value :fn-call [{:keys [args]} opts]
  (let [compiled-args (map #(compile-value % opts) args)
        deps (set (mapcat :deps compiled-args))
        value (map :inline-value compiled-args)]
    {:deps deps
     :inline-value value}))

(defmethod compile-call-value :fn-decl [{:keys [fn-name arities]} opts]
  {:inline-value `(fn ~@(when fn-name [fn-name])
                    ~@(for [{:keys [args body]} arities]
                        `(~args ~(:inline-value (compile-value body opts)))))})

(defmethod compile-value :call [call opts]
  (compile-call-value call opts))
