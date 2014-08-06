(ns flow.compile.wrap-lens
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

(alias 'fs (doto 'flow.state create-ns))

(defmethod compile-call-identity :wrap-lens [{:keys [lens]} {:keys [path] :as opts}]
  (let [wrap-sym (u/path->sym "wrap" path (str (gensym (str lens))))
        deps #{lens}]
    
    (reify fp/CompiledIdentity
      (hard-deps [_] nil)
      (soft-deps [_] deps)

      (declarations [_]
        [`(defn ~wrap-sym []
            (let [!atom# (atom (get fs/*state* (quote ~lens)))]
              (letfn [(update-wrapped-atom# []
                        (reset! !atom# (get fs/*state* (quote ~lens)))
                        [!atom# update-wrapped-atom#])]
                
                [!atom# update-wrapped-atom#])))])

      (build-form [_]
        `(~wrap-sym)))))
