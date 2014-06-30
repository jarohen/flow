(ns flow.compile.calls
  (:require [flow.compile :refer [compile-el]]))

(defmulti compile-call
  (fn [call opts]
    (:call-type call)))

(defmethod compile-call :do [{:keys [side-effects return]} opts]
  (let [compiled-return (compile-el return opts)]
    {:el-init `[~@side-effects ~@(:el-init compiled-return)]

     :deps (:deps return)

     ;; TODO not sure about this...
     :as-value (when-let [as-val (:as-value compiled-return)]
                 `(do
                    ~@side-effects
                    ~as-val))
     
     :on-update (:on-update return)}))

(defmethod compile-call :if [{:keys [test then else]} opts]
  {:as-value `(if ~(:as-value test)
                ~(:as-value then)
                ~(:as-value else))})

(defmethod compile-call :let [{:keys [bindings side-effects body]}]
  )

(defmethod compile-call :fn-call [{:keys [args]} opts]
  (let [compiled-args (map #(compile-el % opts) args)
        deps (set (mapcat :deps compiled-args))]
    {:deps deps
     :as-value (map :as-value compiled-args)}))

(defmethod compile-call :unwrap-cursor [{:keys [cursor]}
                                        {:keys [dynamic-syms state-sym updated-var-sym]
                                         :as opts}]
  (let [unshadowed-sym (if-let [{:keys [unshadowed-sym]} (get dynamic-syms cursor)]
                         unshadowed-sym
                         cursor)]
    {:deps #{unshadowed-sym}
     :as-value `(get ~state-sym ~unshadowed-sym)}))

(comment
  (require 'flow.parse)

  (compile-el (flow.parse/parse-form '[:div {:flow.core/style {:color (<<! !color)}}] {:elem? true}) {:state-sym (gensym "state")}))

(defmethod compile-el :call [call opts]
  (compile-call call opts))

