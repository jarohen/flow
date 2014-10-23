(ns flow.forms.node
  (:require #+clj [flow.compiler :as fc]
            [flow.dom.attributes :as fda]
            [flow.dom.children :as fdc]
            [flow.dom.elements :as fde]))

(defn build-node [{:keys [tag style children]}]
  (let [$el (fde/new-element tag)]
    (doseq [child children]
      (let [[$child update-child!] (child)]
        (fdc/append-child! $el $child)))

    (doseq [{:keys [attr value]} style]
      (let [[initial-value update-value!] (value)]
        (fda/set-style! $el attr initial-value)))
    
    (fn update-node! []
      [$el update-node!])))

#+clj
(defn parse-node [[tagish possible-attrs & body]]
  (let [tagish (name tagish)
        attrs (when (map? possible-attrs)
                possible-attrs)

        children (if attrs
                   body
                   (cons possible-attrs body))

        tag (second (re-find #"^([^#.]+)" tagish))]

    {:type :node

     :tag tag
     
     :id (second (re-find #"#([^.]+)" tagish))
     
     :classes (concat (map second (re-seq #"\.([^.]+)" tagish))
                      (:flow.core/classes attrs))

     :style (:flow.core/style attrs)

     :listeners (:flow.core/on attrs)
     
     :attrs (dissoc attrs :flow.core/classes :flow.core/style :flow.core/on)
     
     :children children}))

#+clj
(defn compile-node [{:keys [tag attrs style children]}]
  `(build-node ~{:tag tag

                 :attrs (vec (for [[k v] attrs]
                               {:attr k
                                :value (fc/compile-value-form v)}))

                 :style (vec (for [[k v] style]
                               {:attr k
                                :value (fc/compile-value-form v)}))
                 
                 :children (vec (map fc/compile-el-form children))}))

#+clj
(defmethod fc/compile-el-form :node [node]
  (-> node
      parse-node
      compile-node))
