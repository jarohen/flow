(ns flow.forms.let
  (:require [flow.state :as fs]
            [clojure.set :as set]))

;; So let's say that compiled-bindings each have {:keys [deps update-state]}, where update-state 

(defn init-bindings [compiled-bindings]
  (reduce (fn [{:keys [state current-bindings]} {:keys [deps build-binding value->state]}]
            (let [[initial-value update-fn] (binding [fs/*state* state]
                                              (build-binding))
                  initial-state (value->state initial-value)]
              (prn state)
              (prn initial-state)
              {:state (merge state initial-state)
               :current-bindings (conj current-bindings [initial-state update-fn])}))
          
          {:state fs/*state*
           :current-bindings []}

          compiled-bindings))

(defn build-let [compiled-bindings compiled-body]
  (let [{:keys [state current-bindings]} (init-bindings compiled-bindings)]
    (prn state "<-- state after bindings")
    (binding [fs/*state* state]
      [(first ((:build-fn compiled-body))) nil])))
