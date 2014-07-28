(ns flow.util
  (:require [flow.protocols :as fp]
            [clojure.string :as s]))

#+clj
(defn quote-deps [deps]
  (when (seq deps)
    `#{~@(for [dep deps]
           `(quote ~dep))}))

#+clj
(defn with-updated-deps-check [deps updated-vars-sym quoted-then & [quoted-else]]
  (if (seq deps)
    `(if (deps-updated? ~(quote-deps deps) ~updated-vars-sym)
       ~quoted-then
       ~quoted-else)
    
    quoted-else))

#+cljs
(defn deps-updated? [quoted-deps updated-vars]
  (when (seq quoted-deps)
    (boolean (some #(contains? quoted-deps %) updated-vars))))

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
