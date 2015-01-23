(ns flow.forms.node
  (:require #+clj [flow.compiler :as fc]
            [flow.dom.attributes :as fda]
            [flow.dom.children :as fdc]
            [flow.dom.elements :as fde]
            [flow.dom.scheduler :as fds]))

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

(defn update-classes! [$el {:keys [classes class-fns] :as class-state}]
  (when (seq class-fns)
    (let [new-classes (->> (for [class-fn class-fns]
                             (let [new-value (class-fn)]
                               (if (coll? new-value)
                                 new-value
                                 [new-value])))
                           (apply concat)
                           (remove nil?)
                           set)

          new-class-state {:classes classes
                           :class-fns class-fns}]
      
      (when-not (= new-classes classes)
        (fds/schedule-dom-change
         (fn []
           (fda/set-classes! $el new-classes))))
      
      new-class-state)))

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

(defn build-node [{:keys [tag id lifecycle-callbacks] :as node}]
  (fn []
    (let [$el (fde/new-element tag)]
      (when id
        (fda/set-id! $el id))

      (let [compiled-node (letfn [(update-node! [{:keys [attrs styles children classes listeners]}]
                                    (let [updated-children (update-children! children)
                                          updated-attrs (update-attrs! $el attrs)
                                          updated-styles (update-styles! $el styles)
                                          updated-classes (update-classes! $el classes)
                                          updated-listeners (update-listeners! listeners)]
                                      
                                      [$el #(update-node! {:attrs updated-attrs
                                                           :styles updated-styles
                                                           :children updated-children
                                                           :classes updated-classes
                                                           :listeners updated-listeners})]))]
                            
                            (update-node! (-> node
                                              (update-in [:children] #(with-child-holders $el %))
                                              (update-in [:listeners] #(build-listeners! $el %)))))]

        (when-let [on-mount (get lifecycle-callbacks :mount)]
          (on-mount $el))

        compiled-node))))

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

     :listeners (->> (:flow.core/on attrs)
                     (remove (comp #{"flow.core"} namespace key))
                     (into {}))

     :lifecycle-callbacks (->> (:flow.core/on attrs)
                               (filter (comp #{"flow.core"} namespace key))
                               (map #(update-in % [0] (comp keyword name)))
                               (into {}))
     
     :attrs (dissoc attrs :flow.core/classes :flow.core/style :flow.core/on)
     
     :children children}))

#+clj
(defn compile-node [{:keys [tag id attrs styles classes listeners lifecycle-callbacks children]} opts]
  `(build-node ~{:tag tag

                 :id id

                 :classes {:class-fns (vec (for [class-form classes]
                                             `(fn []
                                                ~(fc/compile-value-form class-form opts))))}

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

                 :lifecycle-callbacks lifecycle-callbacks
                 
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
