(ns flow.render)

(defn schedule-rendering-frame [f]
  (if (exists? js/requestAnimationFrame)
    (js/requestAnimationFrame f)
    (js/setTimeout f 0)))
