(ns flow.forms.node
  (:require #+clj [flow.compiler :as fc]
            [flow.dom.attributes :as fda]
            [flow.dom.children :as fdc]
            [flow.dom.elements :as fde]))

(defn update-style! [$el style]
  (->> (for [{:keys [attr value-fn] :as style-attr} style]
         (let [new-value (value-fn)]
           (if-not (= new-value (get style-attr :previous-value ::nil))
             (do
               (fda/set-style! $el attr new-value)
               (assoc style-attr :previous-value new-value))
                
             style-attr)))
       doall))

(defn update-children! [$el children]
  (fdc/clear! $el)
  
  (->> (for [child children]
         (let [[$child update-child!] (child)]
           (fdc/append-child! $el $child)
           
           update-child!))
       
       doall))

(defn build-node [{:keys [tag] :as node}]
  (fn []
    (let [$el (fde/new-element tag)]
      (letfn [(update-node! [{:keys [style children]}]
                (let [updated-style (update-style! $el style)
                      updated-children (update-children! $el children)]
                  
                  [$el #(update-node! {:style updated-style
                                       :children updated-children})]))]
        (update-node! node)))))

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
