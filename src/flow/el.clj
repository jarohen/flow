(ns flow.el
  (:require [flow.expand :refer [expand-macros]]
            [flow.parse :refer [parse-form]]
            [flow.compile :refer [compile-el]]
            [flow.render :refer [render-el]]))

(defmacro el [elem]
  (let [el-sym (gensym "flow-el")
        syms {:state-sym (symbol (str el-sym "-state"))
              :old-state-sym (symbol (str el-sym "-old-state"))
              :new-state-sym (symbol (str el-sym "-new-state"))
              :updated-vars-sym (symbol (str el-sym "-updated-vars"))}]
    (-> (expand-macros elem &env)
        (parse-form {:elem? true
                     :path (str el-sym)})
        (compile-el syms)
        (doto (->> (spit "/tmp/compiled.edn")))
        (render-el syms)
        (doto (->> (spit "/tmp/rendered.edn"))))))

#_(comment
    (let [el-sym (gensym "flow-el")
          syms {:state-sym (symbol (str el-sym "-state"))
                :old-state-sym (symbol (str el-sym "-old-state"))
                :new-state-sym (symbol (str el-sym "-new-state"))
                :updated-vars-sym (symbol (str el-sym "-updated-vars"))}]
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
