(ns flow.render
  (:require [flow.protocols :as fp]))

(alias 'fd (doto 'flow.dom create-ns))

(defn render-form [compiled-form path]
  (let [!state (gensym "!state")
        notify! (gensym "notify!")
        handler (gensym "handler")

        !el (gensym "!el")
        
        state (symbol (str path "-state"))
        new-state (symbol (str path "-new-state"))
        updated-vars (symbol (str path "-updated-vars"))

        deps (fp/form-deps compiled-form)]

    `(do
       (let [~!state (atom {})
             ~!el (atom nil)
             ~@(apply concat (fp/bindings compiled-form))]
                  
         (letfn [(~notify! [~new-state ~updated-vars]
                   (let [$old-el# @~!el
                         $new-el# (fd/->node ~(fp/updated-value-form compiled-form new-state updated-vars))]
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
                 $initial-el# (fd/->node ~(fp/initial-value-form compiled-form state))]
             
             (reset! ~!el $initial-el#)
             $initial-el#))))))
