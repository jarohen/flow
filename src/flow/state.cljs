(ns flow.state)

(def ^:dynamic *state* {})

(defn with-state-closure [f]
  (let [closed-state *state*]
    (fn [& args]
      (binding [*state* closed-state]
        (apply f args)))))
