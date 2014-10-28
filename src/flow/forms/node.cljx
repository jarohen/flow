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

(defn update-classes! [$el classes]
  (when (seq classes)
    (let [new-classes (for [{:keys [class-value-fn] :as class} classes]
                        {:previous-values (let [new-value (class-value-fn)]
                                            (if (coll? new-value)
                                              new-value
                                              [new-value]))
                         :class-value-fn class-value-fn})
          new-classes-set (->> (mapcat :previous-values new-classes)
                               (remove nil?)
                               set)]
      (if (= new-classes-set (->> (mapcat :previous-values classes)
                                  (remove nil?)
                                  set))
        classes

        (do
          (fda/set-classes! $el new-classes-set)
          new-classes)))))

(defn with-child-holders [$parent children]
  (for [child children]
    {:child child
     :holder (fdc/new-child-holder! $parent)}))

(defn update-children! [children]
  (->> (for [{:keys [child holder]} children]
         (let [[$child update-child!] (child)]
           (fdc/replace-child! holder $child)
           
           {:child update-child!
            :holder holder}))
       
       doall))

(defn build-listeners! [$el listeners]
  (->> (for [{:keys [event build-listener]} listeners]
         (let [initial-listener (build-listener)
               !listener (atom initial-listener)]
           (fde/add-event-listener! $el event (fn [e]
                                                (when-let [listener @!listener]
                                                  (listener e))))
           {:!listener !listener
            :build-listener build-listener}))
       doall))

(defn update-listeners! [listeners]
  (doseq [{:keys [!listener build-listener]} listeners]
    (reset! !listener (build-listener)))
  
  listeners)

(defn build-node [{:keys [tag id] :as node}]
  (fn []
    (let [$el (fde/new-element tag)]
      (when id
        (fda/set-id! $el id))

      (letfn [(update-node! [{:keys [attrs styles children classes listeners]}]
                (let [updated-attrs (update-attrs! $el attrs)
                      updated-styles (update-styles! $el styles)
                      updated-children (update-children! children)
                      updated-classes (update-classes! $el classes)
                      updated-listeners (update-listeners! listeners)]
                  
                  [$el #(update-node! {:attrs updated-attrs
                                       :styles updated-styles
                                       :children updated-children
                                       :classes updated-classes
                                       :listeners updated-listeners})]))]
        
        (update-node! (-> node
                          (update-in [:children] #(with-child-holders $el %))
                          (update-in [:listeners] #(build-listeners! $el %))))))))

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

    {:tag tag
     
     :id (second (re-find #"#([^.]+)" tagish))
     
     :classes (->> (:flow.core/classes attrs)
                   (cons (mapv (comp keyword second) (re-seq #"\.([^.]+)" tagish)))
                   (remove empty?))

     :styles (:flow.core/style attrs)

     :listeners (:flow.core/on attrs)
     
     :attrs (dissoc attrs :flow.core/classes :flow.core/style :flow.core/on)
     
     :children children}))

#+clj
(defn compile-node [{:keys [tag id attrs styles classes listeners children]} opts]
  `(build-node ~{:tag tag

                 :id id

                 :classes (vec (for [class-form classes]
                                 {:class-value-fn `(fn []
                                                     ~(fc/compile-value-form class-form opts))}))

                 :attrs (vec (for [[k v] attrs]
                               {:attr-key k
                                :value-fn `(fn []
                                             ~(fc/compile-value-form v opts))}))

                 :styles (vec (for [[k v] styles]
                                {:style-key k
                                 :value-fn `(fn []
                                              ~(fc/compile-value-form v opts))}))

                 :listeners (vec (for [[event listener] listeners]
                                   {:event event
                                    :build-listener `(fn []
                                                       ~(fc/compile-value-form listener opts))}))
                 
                 :children (vec (map #(fc/compile-el-form % opts) children))}))

#+clj
(defmethod fc/compile-el-form :node [node opts]
  (-> node
      parse-node
      (compile-node opts)))

(comment
  (let [!number (atom 4)]
    (binding [flow.state/*state* {'!number !number}]
      (let [[$el update!] ((eval (fc/compile-el-form
                                  '[:div {:flow.core/on {:click (fn [e]
                                                                  (println (pr-str e) "happened!"
                                                                           "!number was" (<< !number)))}}]
                                  {:bound-syms #{'!number}})))
            listener-1 (:click (:listeners @$el))]

      
        (listener-1 {:a 1 :b 2})
      
        (listener-1 {:a 3 :b 2})

        (reset! !number 5)

        (let [listener-2 (:click (:listeners @$el))]
          ((:click (:listeners @$el)) {:a 3 :b 2})

          (println '(= listener-1 listener-2) (= listener-1 listener-2)))))))
