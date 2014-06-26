(ns flow.core
  (:require [flow.el :as el :include-macros true]))

#+clj
(defmacro el [elem]
  `(el/el ~elem))

#+cljs
(defn root [$container $elem]
  (loop []
    (when-let [$child (.-firstChild $container)]
      (.removeChild $container $child)
      (recur)))
        
  (.appendChild $container $elem))
