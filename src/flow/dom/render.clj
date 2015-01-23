(ns flow.dom.render)

(defn asynchronously [f]
  (println "'asynchronously' not properly supported in Clojure yet.")
  (f))

(defn schedule-rendering-frame [f]
  (asynchronously f))

