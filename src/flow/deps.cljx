(ns flow.deps
  (:require [flow.lenses.core :refer [->lens]]
            [flow.lenses.common :refer [Lens -!state -path]]))

(defprotocol Context
  (-read-dep [_ dep]))

(def ^:dynamic *ctx*
  (reify Context
    (-read-dep [this dep]
      (->lens @dep dep []))))

(defn mark-dep [dep-tree dep value]
  (let [state+path (if (satisfies? Lens dep)
                     (cons (-!state dep) (-path dep))
                     [dep])]
    (letfn [(dep-marked? [dep-tree path]
              (or (boolean (::value dep-tree))
                  (when-let [[p & more] (seq path)]
                    (dep-marked? (get dep-tree p) more))))]
      (if (dep-marked? dep-tree state+path)
        dep-tree
        (assoc-in dep-tree state+path {::value value})))))

(defn tree-unchanged? [new-value tree]
  (if-let [old-value (::value tree)]
    (identical? old-value new-value)
    (every? (fn [[path sub-tree]]
              (tree-unchanged? (get new-value path) sub-tree))
            tree)))

(defn deps-unchanged? [deps]
  (every? (fn [[!atom tree]]
            (tree-unchanged? @!atom tree))
          deps))

(defn with-watch-context [f]
  (let [!dep-tree (atom {})
        parent-ctx *ctx*]
    (binding [*ctx* (reify Context
                      (-read-dep [_ dep]
                        (let [value (-read-dep parent-ctx dep)]
                          (swap! !dep-tree mark-dep dep value)
                          value)))]

      {:result (f)
       :deps @!dep-tree})))

(defn read-dep [dep]
  (-read-dep *ctx* dep))

