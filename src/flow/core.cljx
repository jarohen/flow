(ns flow.core
  #+clj (:require [flow.el :as el])
  #+cljs (:require flow.protocols
                   flow.dom
                   flow.if
                   flow.let
                   flow.for
                   flow.symbol))

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
