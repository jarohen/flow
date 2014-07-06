(ns flow.el
  (:require [flow.expand :refer [expand-macros]]
            [flow.parse :refer [parse-form]]
            [flow.compile :refer [compile-el]]
            [flow.render :refer [render-el]]))

(defmacro el [elem]
  (let [el-sym (gensym "flow-el")
        syms {:state (symbol (str el-sym "-state"))
              :old-state (symbol (str el-sym "-old-state"))
              :new-state (symbol (str el-sym "-new-state"))
              :updated-vars (symbol (str el-sym "-updated-vars"))
              :dynamic-syms #{}
              :local-syms #{}}]
    
    (-> (expand-macros elem &env)
        (parse-form {:elem? true
                     :path (str (gensym "flow-el"))})
        (doto (->> (spit "/tmp/parsed.edn")))
        (compile-el syms)
        (doto (->> (spit "/tmp/compiled.edn")))
        (render-el)
        (doto (->> (spit "/tmp/rendered.edn"))))))
