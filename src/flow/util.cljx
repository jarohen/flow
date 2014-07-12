(ns flow.util)

(defn quote-deps [deps]
  (when (seq deps)
    `#{~@(for [dep deps]
           `(quote ~dep))}))

(defn deps-updated? [quoted-deps updated-vars]
  (when (seq quoted-deps)
    (boolean (some #(contains? quoted-deps %) updated-vars))))
