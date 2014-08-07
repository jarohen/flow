(ns flow.todomvc.ui.todomvc-model
  (:require [cljs.core.async :as a])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

;; This is pure Clojure - test it in a normal REPL!

(defmulti apply-event #(:type %2))

(defmethod apply-event :toggle [todos {:keys [toggled-id]}]
  (update-in todos [toggled-id :done?] not))

(defmethod apply-event :update [todos {:keys [updated-id caption]}]
  (assoc-in todos [updated-id :caption] caption))

(defmethod apply-event :new-todo [todos {:keys [caption]}]
  (let [new-id ((fnil inc 0) (apply max (keys todos)))]
    (assoc todos
      new-id {:caption caption, :done? false})))

(defmethod apply-event :delete [todos {:keys [deleted-id]}]
  (dissoc todos deleted-id))

(defmethod apply-event :clear-completed [todos _]
  (->> todos
       (remove (comp :done? val))
       (into {})))

(defmethod apply-event :toggle-all [todos {:keys [done?]}]
  (->> todos
       (map #(assoc-in % [1 :done?] done?))
       (into {})))

(defmethod apply-event nil [todos _]
  todos)

(defn watch-events! [events-ch !todos]
  (go-loop []
    (when-let [event (a/<! events-ch)]
      (swap! !todos apply-event event)
      (recur))))



