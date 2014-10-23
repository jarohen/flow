(ns flow.forms.node
  (:require #+clj [flow.compiler :as fc]
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
(defn compile-node [{:keys [tag attrs style children]}]
  `(build-node ~{:tag tag

                 :children (vec (map fc/compile-el-form children))}))

#+clj
(defmethod fc/compile-el-form :node [node]
  (-> node
      parse-node
      compile-node))
