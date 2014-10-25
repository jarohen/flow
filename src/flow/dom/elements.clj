(ns flow.dom.elements)

(defn text-el [s]
  (atom {:text s}))

(defn new-element [tag]
  (atom {:tag tag}))

