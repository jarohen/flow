(ns flow.el
  (:require [flow.dom.children :as fdc]))

(defn root [$container build-el]
  (let [[$el update-el!] (build-el)]
    (fdc/clear! $container)
    (fdc/append-child! $container $el)))

(defn render-el [el]
  (el))
