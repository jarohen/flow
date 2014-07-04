(ns flow.compile.update)

(defn on-update-form
  ([{:keys [updated-var-sym] :as opts}]
     (fn [{:keys [deps on-update] :as form}]
       (on-update-form form opts)))
  
  ([{:keys [deps on-update]} {:keys [updated-var-sym]}]
     (when (seq deps)
       `(when (contains? #{~@(for [dep deps]
                               `(quote ~dep))
                           :all}
                         ~updated-var-sym)
          ~@on-update))))
