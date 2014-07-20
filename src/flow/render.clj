(ns flow.render
  (:require [flow.protocols :as fp]))

(defn render-form [compiled-form path]
  (let [!state (gensym "!state")
        notify! (gensym "notify!")
        handler (gensym "handler")
        
        state (symbol (str path "-state"))
        old-state (symbol (str path "-old-state"))
        new-state (symbol (str path "-new-state"))
        updated-vars (symbol (str path "-updated-vars"))

        deps (fp/form-deps compiled-form)]

    `(do
       (let [~!state (atom {})
             ~@(apply concat (fp/bindings compiled-form))]
                  
         (letfn [(~notify! [~old-state ~new-state ~updated-vars]
                   ~(fp/updated-value-form compiled-form old-state new-state updated-vars))
                 
                 (~handler [dep-sym#]
                   (fn [_1# _2# old-value# new-value#]
                     (when (not= old-value# new-value#)
                       (let [~old-state @~!state
                             ~new-state (swap! ~!state assoc dep-sym# new-value#)]
                         (~notify! ~old-state ~new-state #{dep-sym#})))))]

           ~@(for [dep deps]
               `(do
                  (swap! ~!state assoc (quote ~dep) (deref ~dep))
                  (add-watch ~dep ~(str (gensym "watch")) (~handler (quote ~dep)))))

           (let [~state @~!state]
             ~(fp/initial-value-form compiled-form state)))))))
