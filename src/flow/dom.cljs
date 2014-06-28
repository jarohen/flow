(ns flow.dom)

(defn add-class! [$el class-name]
  (.. $el
      -classList
      (add class-name)))
