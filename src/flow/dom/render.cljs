(ns flow.dom.render)

(defn asynchronously [f]
  (js/setTimeout f 0))

(defn schedule-rendering-frame [f]
  ;; Run asynchronously
  (if (exists? js/requestAnimationFrame)
    (js/requestAnimationFrame f)
    (asynchronously f)))


