(ns flow.dom.elements)

(defn text-el [s]
  {:text s})

(defn new-element [tag]
  (atom {:tag tag}))

