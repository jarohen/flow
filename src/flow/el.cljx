(ns flow.el
  (:require [flow.dom.children :as fdc]))

(defn root [$container build-el]
  (let [[$el update-el!] (build-el)]
    (fd/clear! $container)
    (fd/append-child! $container $el)))

(defn render-el [el]
  (el))
