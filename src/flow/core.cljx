(ns flow.core
  #+clj (:require [flow.expand :refer [expand-macros]]
                  [flow.parse :refer [parse-form]]
                  [flow.compile :refer [compile-identity]]
                  [flow.render :refer [render-elem]]
                  [flow.protocols :as fp])
  
  #+cljs (:require flow.state
                   flow.render
                   flow.lens
                   flow.util
                   flow.forms.if
                   flow.forms.let
                   flow.forms.fn-call
                   flow.forms.case
                   flow.forms.for
                   [flow.dom :as fd]))

#+clj
(defn debug-compiled-el [compiled-el]
  (spit "/tmp/compiled.edn"
        {:hard-deps (fp/hard-deps compiled-el)
         :soft-deps (fp/soft-deps compiled-el)
         :declarations (fp/declarations compiled-el)
         :build-form (fp/build-form compiled-el)}))

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
        (render-elem)
        (doto (->> (spit "/tmp/rendered.edn"))))))

#+cljs
(defn root [$container $elem]
  (loop []
    (when-let [$child (.-firstChild $container)]
      (.removeChild $container $child)
      (recur)))
        
  (.appendChild $container $elem))

#+cljs
(defn bind-value! [lens]
  (fd/bind-value! lens))
