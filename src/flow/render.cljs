(ns flow.render)

(defn schedule-rendering-frame [f]
  ;; Run asynchronously
  (js/setTimeout f 0))
