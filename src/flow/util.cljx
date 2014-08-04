(ns flow.util
  (:require [flow.protocols :as fp]))

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

