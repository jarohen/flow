(ns flow.render
  (:require [flow.protocols :as fp]
            [flow.util :as u]))

(alias 'fd (doto 'flow.dom create-ns))

(defn render-el [compiled-elem path]
  (let [!state (u/path->sym "!" path "state")
        notify! (u/path->sym path "notify!")
        handler (u/path->sym path "handler")

        !el (u/path->sym "!" path "el")
        
        state (u/path->sym path "state")
        new-state (u/path->sym path "new-state")
        updated-vars (u/path->sym path "updated-vars")

        deps (fp/identity-deps compiled-elem)]

    `(do
       (let [~!state (atom {})
             ~!el (atom nil)
             ~@(apply concat (fp/bindings compiled-elem))]
                  
         (letfn [(~notify! [~new-state ~updated-vars]
                   (let [$old-el# @~!el
                         $new-el# (fd/->node ~(fp/updated-form compiled-elem new-state updated-vars))]
                     (when-not (= $old-el# $new-el#)
                       (fd/swap-elems! $old-el# $new-el#))))
                 
                 (~handler [dep-sym#]
                   (fn [~'_ ~'_ old-value# new-value#]
                     (when (not= old-value# new-value#)
                       (let [~new-state (swap! ~!state assoc dep-sym# new-value#)]
                         (~notify! ~new-state #{dep-sym#})))))]

           ~@(for [dep deps]
               `(do
                  (swap! ~!state assoc (quote ~dep) (deref ~dep))
                  (add-watch ~dep ~(str (gensym "watch")) (~handler (quote ~dep)))))

           (let [~state @~!state
                 $initial-el# (fd/->node ~(fp/initial-form compiled-elem state))]
             
             (reset! ~!el $initial-el#)
             $initial-el#))))))
