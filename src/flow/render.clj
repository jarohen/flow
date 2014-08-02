(ns flow.render
  (:require [flow.protocols :as fp]
            [flow.util :as u]))

(alias 'fd (doto 'flow.dom create-ns))

(defn render-elem [compiled-elem]
  `(do
     ~@(fp/declarations compiled-elem)
     (flow.render/build-el {:deps [~@(when (satisfies? fp/CompiledIdentity compiled-elem)
                                      (for [dep (set (concat (fp/hard-deps compiled-elem)
                                                             (fp/soft-deps compiled-elem)))]
                                        `{:dep-sym (quote ~dep)
                                          :dep ~dep}))]
                           :build-fn #(~@(fp/build-form compiled-elem))})))
