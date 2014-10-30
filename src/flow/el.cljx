(ns flow.el
  (:require [flow.dom.children :as fdc]
            [flow.dom.elements :as fde]
            [flow.state :as fs]
            [flow.render :as fr]
            [clojure.set :as set]))

(defn root-ctx []
  (reify fs/Context
    (-read-lens [_ lens]
      @lens)
    (-peek-lens [_ lens]
      @lens)))

(defn with-watch-context [parent-ctx f]
  (let [!deps (atom #{})]
    (binding [fs/*ctx* (reify fs/Context
                         (-read-lens [_ lens]
                           (swap! !deps conj lens)
                           (fs/-read-lens parent-ctx lens))
                         (-peek-lens [_ lens]
                           (fs/-peek-lens parent-ctx lens)))]

      {:result (f)
       :deps @!deps})))

(defn update-watches! [{:keys [old-deps new-deps on-change watch-id]}]
  (doseq [old-dep (set/difference old-deps new-deps)]
    (remove-watch old-dep watch-id))

  (doseq [new-dep (set/difference new-deps old-deps)]
    (add-watch new-dep watch-id
               (fn [_ _ old new]
                 (when-not (identical? old new)
                   (on-change))))))

(defn root [$container el]
  (let [!deps (atom #{})
        !child (atom el)
        el-holder (fdc/new-child-holder! $container)
        !dirty? (atom true)
        watch-id (gensym "watch")]
    (letfn [(update-root! []
              (fr/schedule-rendering-frame
               (fn []
                 (reset! !dirty? false)
                 
                 (let [{:keys [result deps]} (with-watch-context (root-ctx)
                                               (fn []
                                                 (@!child)))]
                   
                   (let [[$child update-child!] result]
                     (reset! !child update-child!)
                     (fdc/replace-child! el-holder $child))

                   (let [old-deps @!deps]
                     (update-watches! {:old-deps old-deps
                                       :new-deps deps
                                       :watch-id watch-id
                                       :on-change (fn []
                                                    (when (compare-and-set! !dirty?
                                                                            false
                                                                            true)
                                                      (update-root!)))})
                     (reset! !deps deps))

                   $container))))]
      
      (update-root!))))

(defn render-el [build-el]
  (fn []
    (letfn [(update-el! [{:keys [update-component! $el deps dep-values]}]
              
              (let [new-dep-values (->> deps
                                        (map (juxt identity fs/peek-lens))
                                        (into {}))

                    deps-unchanged? (and update-component!
                                         (->> (for [dep deps]
                                                (identical? (get dep-values dep ::not-present)
                                                            (get new-dep-values dep ::not-present)))
                                              (every? true?))) 
                    
                    {:keys [result deps]} (or (when deps-unchanged?
                                                {:result [$el update-component!]
                                                 :deps deps})

                                              (with-watch-context fs/*ctx*
                                                (fn []
                                                  ((or update-component! build-el)))))
                    
                    [$el update-component!] result]
                
                [$el #(update-el! {:$el $el
                                   :update-component! update-component!
                                   :deps deps
                                   :dep-values new-dep-values})]))]
      
      (update-el! {}))))
