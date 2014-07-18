(ns flow.compile.if
  (:require [flow.compile :refer [compile-form]]
            [flow.compile.calls :refer [compile-call-form]]
            [flow.bindings :as b]
            [flow.util :as u]
            [clojure.set :as set]
            [flow.protocols :as fp]))

#_(defmethod compile-call-form :if [{:keys [path test then else]} {:keys [state] :as opts}]
    (let [if-sym (symbol path)
          compiled-test (compile-form test opts)
          [compiled-then compiled-else] (map #(compile-form % opts) [then else])
          deps (mapcat :deps [compiled-test compiled-then compiled-else])]

      {:el `(~if-sym)
       :deps deps
       :declarations (concat (mapcat :declarations [compiled-test compiled-then compiled-else])
                             [`(defn ~if-sym []
                                 (flow.forms.if/if->el ~(u/quote-deps deps)
                                                       (fn [~state]
                                                         ~(:inline-value compiled-test))
                                                       ~(:el compiled-then)
                                                       ~(:el compiled-else)))])}))
