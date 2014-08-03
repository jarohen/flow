(ns flow.util
  (:require [flow.protocols :as fp]
            [clojure.string :as s]))

#+clj (alias 'fs (doto 'flow.state create-ns))

#+clj
(defn quote-deps [deps]
  (when (seq deps)
    `#{~@(for [dep deps]
           `(quote ~dep))}))

#+clj
(defn with-updated-deps-check [deps quoted-then & [quoted-else]]
  (if (seq deps)
    `(if (fs/deps-updated? ~(quote-deps deps))
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

#+cljs
(defn value->build-fn [f]
  (letfn [(update-fn []
            [(f) update-fn])]
    (update-fn)))

#+clj
(defn value->identity [compiled-value]
  (reify fp/CompiledIdentity
    (hard-deps [_] (fp/value-deps compiled-value))
    (soft-deps [_] nil)
    (declarations [_] nil)
    (build-form [_]
      `(value->build-fn (fn []
                          ~(fp/inline-value-form compiled-value))))))
