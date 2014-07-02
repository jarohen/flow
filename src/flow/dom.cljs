(ns flow.dom
  (:require [clojure.set :as set]))

(defn add-class! [$el class-name]
  (.. $el
      -classList
      (add class-name)))

(defn remove-class! [$el class-name]
  (.. $el
      -classList
      (remove class-name)))

(defn add-classes! [$el classes]
  (doseq [class-name classes]
    (add-class! $el class-name)))

(defn update-classes! [$el old-classes new-classes]
  (doseq [class-name (set/difference new-classes old-classes)]
    (add-class! $el class-name))
  
  (doseq [class-name (set/difference old-classes new-classes)]
    (remove-class! $el class-name)))

(defn set-style! [$el k v]
  (aset (.-style $el) (name k) (cond-> v
                                 (keyword? v) name)))

(defn set-attr! [$el k v]
  (if-not (nil? v)
    (.setAttribute $el (name k) v)
    (.removeAttribute $el (name k) v)))
