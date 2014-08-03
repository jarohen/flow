(ns flow.forms.let
  (:require [flow.state :as fs]
            [clojure.set :as set]))

(defn init-bindings [compiled-bindings]
  (reduce (fn [{:keys [state current-bindings]} {:keys [deps build-binding value->state]}]
            (let [[initial-value update-fn] (binding [fs/*state* state]
                                              (build-binding))
                  initial-state (value->state initial-value)]
              {:state (merge state initial-state)
               :current-bindings (conj current-bindings [initial-state update-fn])}))
          
          {:state fs/*state*
           :current-bindings []}

          compiled-bindings))

(defn update-bindings [compiled-bindings current-bindings]
  (reduce (fn [{:keys [state updated-vars new-bindings]} [{:keys [hard-deps deps
                                                                  bound-syms value->state]}
                                                          [current-state update-fn
                                                           :as current-binding]]]
            (if (fs/deps-updated? deps)
              (let [[new-value update-fn] (update-fn)
                    new-state (value->state new-value)]
                {:state (merge state new-state)
                 :updated-vars (if (fs/deps-updated? hard-deps)
                                 (set/union updated-vars bound-syms)
                                 updated-vars)
                 :new-bindings (conj new-bindings
                                     [new-state update-fn])})

              {:state (merge state current-state)
               :updated-vars updated-vars
               :new-bindings (conj new-bindings
                                   [current-state update-fn])}))
          
          {:state fs/*state*
           :updated-vars (fs/updated-vars)
           :new-bindings []}

          (map vector compiled-bindings current-bindings)))

(defn update-let [compiled-bindings current-bindings
                  compiled-body current-value body-update-fn]
  (fn []
    (let [{:keys [state updated-vars new-bindings]} (update-bindings compiled-bindings
                                                                     current-bindings)]
      (binding [fs/*state* (fs/with-updated-vars state updated-vars)]
        (if (fs/deps-updated? (:deps compiled-body))
          (let [[new-value body-update-fn] (body-update-fn)]
            [new-value (update-let compiled-bindings new-bindings
                                   compiled-body new-value body-update-fn)])

          [current-value (update-let compiled-bindings new-bindings
                                     compiled-body current-value body-update-fn)])))))

(defn build-let [compiled-bindings compiled-body]
  (let [{:keys [state current-bindings]} (init-bindings compiled-bindings)]
    (binding [fs/*state* state]
      (let [[initial-value body-update-fn] ((:build-fn compiled-body))]
        [initial-value (update-let compiled-bindings current-bindings
                                   compiled-body initial-value body-update-fn)]))))
