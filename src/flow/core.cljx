(ns flow.core
  #+clj (:require [flow.expand :refer [expand-macros]]
                  [flow.parse :refer [parse-form]]
                  [flow.compile :refer [compile-identity]]
                  [flow.render :refer [render-el]]
                  [flow.protocols :as fp])
  
  #+cljs (:require flow.protocols
                   [flow.dom :as fd]))

#+clj
(defn debug-compiled-el [compiled-el]
  (spit "/tmp/compiled.edn"
        {:deps (fp/identity-deps compiled-el)
         :bindings (fp/bindings compiled-el)
         :initial-value (fp/initial-form compiled-el 'state)
         :updated-value (fp/updated-form compiled-el 'new-state 'updated-vars)}))

#+clj
(defmacro el [elem]
  (let [el-sym (gensym "flow-el")]
    
    (-> (expand-macros elem &env)
        (parse-form {:elem? true})
        (doto (->> (spit "/tmp/parsed.edn")))
        (compile-identity {:dynamic-syms #{}
                           :local-syms #{}
                           :path [el-sym]})
        (doto debug-compiled-el)
        (render-el (str el-sym))
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
