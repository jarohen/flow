(ns flow.forms.node
  (:require #+clj [flow.compiler :as fc]
            [flow.dom.attributes :as fda]
            [flow.dom.children :as fdc]
            [flow.dom.elements :as fde]))

(defn update-attrs! [$el attrs]
  (->> (for [{:keys [attr-key value-fn] :as attr} attrs]
         (let [new-value (value-fn)]
           (if-not (= new-value (get attr :previous-value ::nil))
             (do
               (fda/set-attr! $el attr-key new-value)
               (assoc attr :previous-value new-value))
                
             attr)))
       doall))

(defn update-styles! [$el styles]
  (->> (for [{:keys [style-key value-fn] :as style} styles]
         (let [new-value (value-fn)]
           (if (= new-value (get style :previous-value ::nil))
             style
                
             (do
               (fda/set-style! $el style-key new-value)
               (assoc style :previous-value new-value)))))
       doall))

(defn update-children! [$el children]
  (fdc/clear! $el)
  
  (->> (for [child children]
         (let [[$child update-child!] (child)]
           (fdc/append-child! $el $child)
           
           update-child!))
       
       doall))

(defn build-node [{:keys [tag id] :as node}]
  (fn []
    (let [$el (fde/new-element tag)]
      (when id
        (fda/set-id! $el id))
      
      (letfn [(update-node! [{:keys [attrs styles children]}]
                (let [updated-attrs (update-attrs! $el attrs)
                      updated-styles (update-styles! $el styles)
                      updated-children (update-children! $el children)]
                  
                  [$el #(update-node! {:attrs updated-attrs
                                       :styles updated-styles
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

     :styles (:flow.core/style attrs)

     :listeners (:flow.core/on attrs)
     
     :attrs (dissoc attrs :flow.core/classes :flow.core/style :flow.core/on)
     
     :children children}))

#+clj
(defn compile-node [{:keys [tag id attrs styles children]} opts]
  `(build-node ~{:tag tag

                 :id id

                 :attrs (vec (for [[k v] attrs]
                               {:attr-key k
                                :value-fn `(fn [] ~(fc/compile-value-form v opts))}))

                 :styles (vec (for [[k v] styles]
                                {:style-key k
                                 :value-fn `(fn [] ~(fc/compile-value-form v opts))}))
                 
                 :children (vec (map #(fc/compile-el-form % opts) children))}))

#+clj
(defmethod fc/compile-el-form :node [node opts]
  (-> node
      parse-node
      (compile-node opts)))
