(ns flow.compile.wrap-lens
  (:require [flow.compile.calls :refer [compile-call-identity compile-call-value]]
            [flow.protocols :as fp]
            [flow.util :as u]))

(alias 'fs (doto 'flow.state create-ns))
(alias 'fl (doto 'flow.lens create-ns))

(defmethod compile-call-identity :wrap-lens [{:keys [lens extra-path]} {:keys [path] :as opts}]
  (let [wrap-sym (u/path->sym "wrap" path (str (gensym (str lens))))
        deps #{lens}]
    
    (reify fp/CompiledIdentity
      (hard-deps [_] nil)
      (soft-deps [_] deps)

      (declarations [_]
        [`(defn ~wrap-sym []
            (let [wrapped-lens# (fl/wrap-lens (get fs/*state* (quote ~lens)) ~extra-path)]
              (letfn [(update-wrapped-lens# []
                        [wrapped-lens# update-wrapped-lens#])]
                
                [wrapped-lens# update-wrapped-lens#])))])

      (build-form [_]
        `(~wrap-sym)))))
