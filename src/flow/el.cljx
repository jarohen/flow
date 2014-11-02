(ns flow.el
  (:require [flow.dom.children :as fdc]
            [flow.dom.elements :as fde]
            [flow.deps :as fd]
            [flow.lenses.common :refer [Lens]]
            [flow.state :as fs]
            [flow.render :as fr]
            [clojure.set :as set]))

(defn update-watches! [{:keys [old-deps new-deps on-change watch-id]}]
  (let [old-atoms (set (keys old-deps))
        new-atoms (set (keys new-deps))]
    (doseq [old-atom (set/difference old-atoms new-atoms)]
      (remove-watch old-atom watch-id))

    (doseq [new-atom (set/difference new-atoms old-atoms)]
      (add-watch new-atom watch-id
                 (fn [_ _ old new]
                   (when-not (identical? old new)
                     (on-change)))))))

(defn root [$container el]
  (let [!deps (atom {})
        !child (atom el)
        el-holder (fdc/new-child-holder! $container)
        !dirty? (atom true)
        watch-id (gensym "watch")]
    (letfn [(update-root! []
              (fr/schedule-rendering-frame
               (fn []
                 (reset! !dirty? false)
                 
                 (let [{:keys [result deps]} (fd/with-watch-context
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
    (letfn [(update-el! [{:keys [update-component! $el deps]}]
              (let [{:keys [result deps]} (or (when (and update-component!
                                                         (fd/deps-unchanged? deps))

                                                {:result [$el update-component!]
                                                 :deps deps})

                                              (fd/with-watch-context
                                                (fn []
                                                  ((or update-component! build-el)))))
                    
                    [$el update-component!] result]
                
                [$el #(update-el! {:$el $el
                                   :update-component! update-component!
                                   :deps deps})]))]
      
      (update-el! {}))))
