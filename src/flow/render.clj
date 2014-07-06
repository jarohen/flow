(ns flow.render)

(defn render-el [{:keys [deps el-bindings el-init el-return on-update]} {:keys [updated-vars-sym old-state-sym new-state-sym]}]
  `(do
     (let [~@(apply concat el-bindings)]
       (do ~@el-init)

       ~(when (seq deps)
          (let [!state (gensym "!state")
                watch-key (gensym "watch-key")
                handler (gensym "handler")]
            `(let [~!state (atom {})
                   ~watch-key (gensym "el")]
               
               (letfn [(notify!# [~updated-vars-sym ~old-state-sym ~new-state-sym]
                         ~@on-update)
                       
                       (~handler [dep-sym#]
                         (fn [_1# _2# old-value# new-value#]
                           (when (not= old-value# new-value#)
                             (let [old-state# @~!state
                                   new-state# (swap! ~!state assoc dep-sym# new-value#)]
                               (notify!# #{dep-sym#} old-state# new-state#)))))]
                 
                 ~@(for [dep deps]
                     `(do
                        (add-watch ~dep ~watch-key (~handler (quote ~dep)))
                        (swap! ~!state assoc (quote ~dep) (deref ~dep))))

                 (notify!# #{:all} {} @~!state)))))

       ~el-return)))
