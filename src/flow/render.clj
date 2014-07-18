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

        bindings (fp/bindings compiled-form)
        deps (fp/form-deps compiled-form)]

    `(do
       ~@(fp/form-declarations compiled-form)
       
       (let [~!state (atom {})
             ~@bindings]

         (letfn [(~notify! [~old-state ~new-state ~updated-vars]
                   ~(when (seq bindings)
                      `(when ~(fp/should-update-form compiled-form updated-vars)
                         ~(fp/handle-update-form compiled-form old-state new-state updated-vars))))
                       
                 (~handler [dep-sym#]
                   (fn [_1# _2# old-value# new-value#]
                     (when (not= old-value# new-value#)
                       (let [~old-state @~!state
                             ~new-state (swap! ~!state assoc dep-sym# new-value#)]
                         (~notify! #{dep-sym#} ~old-state ~new-state)))))]
           
           ~@(for [dep deps]
               `(do
                  (add-watch ~dep ~(str (gensym "watch")) (~handler (quote ~dep)))
                  (swap! ~!state assoc (quote ~dep) (deref ~dep))))

           (let [~state @~!state]
             ~(fp/current-value-form compiled-form state)))))))
