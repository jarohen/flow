(ns flow.dom)

(defn add-class! [$el class-name]
  (.. $el
      -classList
      (add class-name)))

(defn remove-class! [$el class-name]
  (.. $el
      -classList
      (remove class-name)))

(defn set-style! [$el k v]
  (aset (.-style $el) (name k) (cond-> v
                                 (keyword? v) name)))

(defn set-attr! [$el k v]
  (if-not (nil? v)
    (.setAttribute $el (name k) v)
    (.removeAttribute $el (name k) v)))
