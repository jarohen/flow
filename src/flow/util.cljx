(ns flow.util
  (:require [flow.protocols :as fp]
            [clojure.string :as s]
            #+cljs [flow.state :as fs]))

#+clj (alias 'fs (doto 'flow.state create-ns))

#+clj
(defn quote-deps [deps]
  (when (seq deps)
    `#{~@(for [dep deps]
           `(quote ~dep))}))

#+cljs
(defn deps-updated? [quoted-deps]
  (when (seq quoted-deps)
    (boolean (some #(contains? quoted-deps %) (:updated-vars fs/*state*)))))

#+clj
(defn with-updated-deps-check [deps quoted-then & [quoted-else]]
  (if (seq deps)
    `(if (deps-updated? ~(quote-deps deps))
       ~quoted-then
       ~quoted-else)
    
    quoted-else))

#+clj
(defn with-more-path [opts more-path]
  (update-in opts [:path] concat more-path))

#+clj
(defn path->sym [& path]
  (->> path
       (mapcat #(if (coll? %) % [%]))
       (map name)
       (s/join "-")
       symbol))
