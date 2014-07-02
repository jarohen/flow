(ns flow.el
  (:require [flow.expand :refer [expand-macros]]
            [flow.parse :refer [parse-form]]
            [flow.compile :refer [compile-el]]
            [flow.render :refer [render-el]]))

(defn print-console-ln [& args]
  (binding [*out* System/out]
    (apply println args)))

(defmacro el [elem]
  (let [syms {:state-sym (gensym "state")
              :old-state-sym (gensym "old-state")
              :new-state-sym (gensym "new-state")
              :updated-var-sym (gensym "updated-var")}]
    (-> (expand-macros elem &env)
        (parse-form {:elem? true})
        (compile-el syms)
        (render-el syms))))

#_(comment
    (let [syms {:state-sym (gensym "state")
                :old-state-sym (gensym "old-state")
                :new-state-sym (gensym "new-state")
                :updated-var-sym (gensym "updated-var")}]
      (-> '[:div#test.container.blah {:flow.core/classes ["abc"]
                                      :data-test "foo"
                                      :flow.core/style {:color (<<! !color)}}
            [:h1 "Hello world!"]
            [:p.copy {:flow.core/style {:text-align :right}}
             "If this works, " [:strong "I'll be very happy :)"]]]
      
          (parse-form {:elem? true})
          (compile-el syms)
          #_(render-el syms)))

    
    )
