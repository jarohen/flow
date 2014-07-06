(ns flow.compile.update)

(defn on-update-form
  ([{:keys [updated-vars-sym] :as opts}]
     (fn [{:keys [deps on-update] :as form}]
       (on-update-form form opts)))
  
  ([{:keys [deps on-update]} {:keys [updated-vars-sym]}]
     (when (seq deps)
       (let [listened-deps `#{~@(for [dep deps]
                                  `(quote ~dep))
                              :all}]
         `(when (some #(contains? ~listened-deps %) ~updated-vars-sym)
            ~@on-update)))))
