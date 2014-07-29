(ns flow.core
  #+clj (:require [flow.expand :refer [expand-macros]]
                  [flow.parse :refer [parse-form]]
                  [flow.compile :refer [compile-el]]
                  [flow.render :refer [render-form]]
                  [flow.protocols :as fp])
  
  #+cljs (:require flow.protocols
                   [flow.dom :as fd]))

#+clj
(defn debug-compiled-form [compiled-form]
  (spit "/tmp/compiled.edn"
        {:deps (fp/form-deps compiled-form)
         :bindings (fp/bindings compiled-form)
         :initial-value (fp/initial-value-form compiled-form 'state)
         :updated-value (fp/updated-value-form compiled-form 'new-state 'updated-vars)}))

#+clj
(defmacro el [elem]
  (let [el-sym (gensym "flow-el")]
    
    (-> (expand-macros elem &env)
        (parse-form {:elem? true
                     :path el-sym})
        (doto (->> (spit "/tmp/parsed.edn")))
        (compile-el {:dynamic-syms #{}
                     :local-syms #{}
                     :path el-sym})
        (doto debug-compiled-form)
        (render-form (str el-sym))
        (doto (->> (spit "/tmp/rendered.edn"))))))

#+cljs
(defn root [$container $elem]
  (loop []
    (when-let [$child (.-firstChild $container)]
      (.removeChild $container $child)
      (recur)))
        
  (.appendChild $container $elem))

#+cljs
(defn bind-value! [cursor]
  (fd/bind-value! cursor))
