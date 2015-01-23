(ns flow.dom.scheduler
  (:require [flow.dom.render :as fr]))

(def ^:dynamic *!outstanding-dom-changes*
  nil)

(defn combine-dom-changes [f]
  (let [!dom-changes (atom [])]
    (binding [*!outstanding-dom-changes* !dom-changes]
      (f))

    (when-let [dom-changes (seq @!dom-changes)]
      (fr/schedule-rendering-frame (fn []
                                     (doseq [change dom-changes]
                                       (change)))))))

(defn schedule-dom-change [f]
  (if *!outstanding-dom-changes*
    (swap! *!outstanding-dom-changes* conj f)
    (f)))
