(ns flow.render)

(alias 'fp (doto 'flow.protocols create-ns))

(defn render-el [{:keys [el deps declarations]}]
  (let [!state (gensym "!state")
        notify! (gensym "notify!")
        handler (gensym "handler")]

    `(do
       ~@declarations
       
       (let [el# ~el
             ~!state (atom {})]

         (letfn [(~notify! [updated-vars# old-state# new-state#]
                   (when (fp/should-update-el? el# updated-vars#)
                     (fp/handle-update! el# old-state# new-state# updated-vars#)))
                       
                 (~handler [dep-sym#]
                   (fn [_1# _2# old-value# new-value#]
                     (when (not= old-value# new-value#)
                       (let [old-state# @~!state
                             new-state# (swap! ~!state assoc dep-sym# new-value#)]
                         (~notify! #{dep-sym#} old-state# new-state#)))))]
           
           ~@(for [dep deps]
               `(do
                  (add-watch ~dep ~(str (gensym "watch")) (~handler (quote ~dep)))
                  (swap! ~!state assoc (quote ~dep) (deref ~dep))))

           (fp/build-element el# @~!state))))))
