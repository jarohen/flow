(ns flow.util)

(defn deps->should-update [deps updated-vars-sym]
  (when (seq deps)
    (let [listened-deps `#{~@(for [dep deps]
                               `(quote ~dep))}]
      `(boolean (some #(contains? ~listened-deps %) ~updated-vars-sym)))))
