(ns flow.forms.for
  (:require [flow.state :as fs]
            [flow.protocols :as fp]
            [clojure.set :as set]))

(defn value->key [{:keys [manual-key-fn]} value]
  (or (when manual-key-fn
        (manual-key-fn value))
      (:flow.core/id value)
      (:id value)
      value))

(defn tree->elements [tree]
  (when tree
    (->> (for [{:keys [element children]} (:values tree)]
           (if element
             [(:current-element element)]
             (tree->elements children)))
         (apply concat))))

(defn init-tree [state ks [{:keys [deps build-binding value->state] :as compiled-binding} & more-bindings] compiled-body]
  (let [[initial-values update-fn] (binding [fs/*state* state]
                                     (build-binding))]
    {:update-fn update-fn
     :current-values initial-values
     :values (for [initial-value initial-values]
               (let [initial-state (merge state (value->state initial-value))
                     value-key (value->key compiled-binding initial-value)]
                 {:value-key value-key
                  :element (when-not (seq more-bindings)
                             (let [[initial-element update-fn] (binding [fs/*state* initial-state]
                                                                 ((:build-fn compiled-body)))]
                               {:value-key (conj ks value-key)
                                :current-element (doto initial-element
                                                   (fp/set-flow-id! (conj ks value-key)))
                                :update-fn update-fn}))
                  
                  :children (when (seq more-bindings)
                              (init-tree initial-state (conj ks value-key) more-bindings))}))}))

(defn update-tree [deps compiled-bindings compiled-body current-tree]
  (fn []
    (letfn [(update-tree* [state ks
                           [{:keys [hard-deps build-binding value->state bound-syms] :as compiled-binding} & more-bindings]
                           {:keys [update-fn current-values values] :as current-tree}]
              (if-not current-tree
                (init-tree state ks (cons compiled-binding more-bindings) compiled-body)
                
                (let [[new-values update-fn] (binding [fs/*state* state]
                                               (update-fn))
                      cache (->> values
                                 (map (juxt :value-key identity))
                                 (into {}))]
                  
                  {:update-fn update-fn
                   :current-values new-values
                   :values (for [new-value new-values]
                             (let [new-state (merge state (value->state new-value))
                                   value-key (value->key compiled-binding new-value)]
                               (if-let [cached-value (get cache value-key)]
                                 {:value-key value-key
                                  :element (when-not (seq more-bindings)
                                             (let [[new-element update-fn] (binding [fs/*state* (fs/with-updated-vars new-state
                                                                                                  (-> (fs/updated-vars)
                                                                                                      (set/difference bound-syms)
                                                                                                      (cond-> (fs/deps-updated? hard-deps) (set/union bound-syms))))]
                                                                             ((get-in cached-value [:element :update-fn])))]
                                               {:deps (:deps compiled-body)
                                                :value-key (conj ks value-key)
                                                :current-element new-element
                                                :update-fn update-fn}))
                                  
                                  :children (comment
                                              (when (seq more-bindings)
                                                (update-tree* initial-state (conj ks value-key) more-bindings)))} 
                                 
                                 {:value-key value-key
                                  :element (when-not (seq more-bindings)
                                             (let [[initial-element update-fn] (binding [fs/*state* new-state]
                                                                                 ((:build-fn compiled-body)))]
                                               {:deps (:deps compiled-body)
                                                :value-key (conj ks value-key)
                                                :current-element (doto initial-element
                                                                   (fp/set-flow-id! (conj ks value-key)))
                                                :update-fn update-fn}))
                                  
                                  :children (comment
                                              (when (seq more-bindings)
                                                (update-tree* initial-state (conj ks value-key) more-bindings nil)))})))})))]

      (let [updated-tree (update-tree* fs/*state* [] compiled-bindings current-tree)]
        [(tree->elements updated-tree) (update-tree deps compiled-bindings compiled-body updated-tree)]))))

(comment
  (let [{:keys [state updated-vars new-bindings]} (update-bindings compiled-bindings
                                                                   current-bindings)]
    (binding [fs/*state* (fs/with-updated-vars state updated-vars)]
      (if (fs/deps-updated? (:deps compiled-body))
        (let [[new-value body-update-fn] (body-update-fn)]
          [new-value (update-for compiled-bindings new-bindings
                                 compiled-body new-value body-update-fn)])

        [current-value (update-for compiled-bindings new-bindings
                                   compiled-body current-value body-update-fn)]))))

(defn build-for [deps compiled-bindings compiled-body]
  (let [initial-tree (init-tree fs/*state* [] compiled-bindings compiled-body)]
    [(tree->elements initial-tree)
     (update-tree deps compiled-bindings compiled-body initial-tree)]))
