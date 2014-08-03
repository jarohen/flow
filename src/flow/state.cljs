(ns flow.state)

(def ^:dynamic *state* {})

(defn with-state-closure [f]
  (let [closed-state *state*]
    (fn [& args]
      (binding [*state* closed-state]
        (apply f args)))))

(defn updated-vars [state]
  (:updated-vars (meta state)))

(defn with-updated-vars [state updated-vars]
  (with-meta state
    {:updated-vars updated-vars}))

(defn deps-updated? [quoted-deps]
  (when (seq quoted-deps)
    (boolean (some #(contains? quoted-deps %) (updated-vars *state*)))))
