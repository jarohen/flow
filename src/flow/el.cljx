(ns flow.el
  (:require [flow.dom.children :as fdc]
            [flow.state :as fs]
            [flow.render :as fr]
            [clojure.set :as set]))

(defn root-ctx []
  (reify fs/Context
    (-read-lens [_ lens]
      @lens)))

(defn with-watch-context [parent-ctx f]
  (let [!deps (atom #{})]
    (binding [fs/*ctx* (reify fs/Context
                         (-read-lens [_ lens]
                           (swap! !deps conj lens)
                           (fs/-read-lens parent-ctx lens)))]

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
        !el (atom el)
        !dirty? (atom true)
        watch-id (gensym "watch")]
    (letfn [(update-root! []
              (fr/schedule-rendering-frame
               (fn []
                 (reset! !dirty? false)
                 (let [{:keys [result deps]} (with-watch-context (root-ctx)
                                               (fn []
                                                 (@!el)))
                       [$el update-el!] result]
                   (reset! !el update-el!)

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

                   (fdc/clear! $container)
                   (fdc/append-child! $container $el)

                   $container))))]
      
      (update-root!))))

(defn render-el [el]
  (fn []
    (let [{:keys [result deps]} (with-watch-context fs/*ctx*
                                  (fn []
                                    (el)))]
      result)))
