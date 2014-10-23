(ns flow.forms.node
  (:require #+clj [flow.compiler :refer [compile-form]]
            [flow.dom.children :as fdc]
            [flow.dom.elements :as fde]))

(defn build-node [{:keys [tag children]}]
  (let [$el (fde/new-element tag)]
    (doseq [child children]
      (let [[$child update-child!] (child)]
        (fdc/append-child! $el $child)))
    (fn update-node! []
      [$el update-node!])))

#+clj
(defn parse-node [node]
  {:tag (first node)
   :children (rest node)})

#+clj
(defn compile-node [{:keys [tag children]}]
  `(build-node ~{:tag tag
                 :children (vec (map compile-form children))}))

#+clj
(defmethod compile-form :node [node]
  (-> node
      parse-node
      compile-node))
