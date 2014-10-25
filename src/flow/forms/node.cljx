(ns flow.forms.node
  (:require #+clj [flow.compiler :as fc]
            [flow.dom.attributes :as fda]
            [flow.dom.children :as fdc]
            [flow.dom.elements :as fde]))

(defn update-style! [$el style]
  (doseq [{:keys [attr value-fn]} style]
    (let [initial-value (value-fn)]
      (fda/set-style! $el attr initial-value))))

(defn update-children! [$el children]
  (fdc/clear! $el)
  
  (doseq [child children]
    (let [[$child update-child!] (child)]
      (fdc/append-child! $el $child))))

(defn build-node [{:keys [tag style children]}]
  (fn []
    (let [$el (fde/new-element tag)]
      (letfn [(update-node! []
                (update-children! $el children)
                (update-style! $el style)
                
                [$el update-node!])]
        (update-node!)))))

#+clj
(defn parse-node [[tagish possible-attrs & body]]
  (let [tagish (name tagish)
        attrs (when (map? possible-attrs)
                possible-attrs)

        children (if attrs
                   body
                   (cond->> body
                     possible-attrs (cons possible-attrs)))

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
(defn compile-node [{:keys [tag attrs style children]} opts]
  `(build-node ~{:tag tag

                 :attrs (vec (for [[k v] attrs]
                               {:attr k
                                :value-fn `(fn [] ~(fc/compile-value-form v opts))}))

                 :style (vec (for [[k v] style]
                               {:attr k
                                :value-fn `(fn [] ~(fc/compile-value-form v opts))}))
                 
                 :children (vec (map #(fc/compile-el-form % opts) children))}))

#+clj
(defmethod fc/compile-el-form :node [node opts]
  (-> node
      parse-node
      (compile-node opts)))
