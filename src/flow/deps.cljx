(ns flow.deps
  (:require [flow.lenses :as fl]))

(defprotocol Context
  (-read-dep [_ dep])
  (-mark-deps! [_ deps]))

(def ^:dynamic *ctx*
  (reify Context
    (-read-dep [_ dep]
      (fl/->lens @dep dep []))
    (-mark-deps! [_ _])))

(defn mark-dep [dep-tree dep value]
  (let [state+path (if (satisfies? fl/Lens dep)
                     (cons (fl/-!state dep) (fl/-path dep))
                     [dep])]
    (letfn [(dep-marked? [dep-tree path]
              (or (boolean (::value dep-tree))
                  (when-let [[p & more] (seq path)]
                    (dep-marked? (get dep-tree p) more))))]
      (if (dep-marked? dep-tree state+path)
        dep-tree
        (assoc-in dep-tree state+path {::value (if (fl/lens? value)
                                                 (fl/-value value)
                                                 value)})))))

(defn merge-deps [deps-1 deps-2]
  (merge-with (fn [v1 v2]
                (cond
                  (::value v1) v1
                  (::value v2) v2
                  :else (merge-deps v1 v2)))
              deps-1 deps-2))

(defn tree-unchanged? [new-value tree]
  (if (and (map? tree)
           (contains? tree ::value))
    (= (::value tree) new-value)

    (every? (fn [[path sub-tree]]
              (tree-unchanged? (fl/get-at-path new-value [path]) sub-tree))
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
                          value))
                      (-mark-deps! [_ deps]
                        (swap! !dep-tree merge-deps deps)
                        (-mark-deps! parent-ctx deps)))]

      {:result (f)
       :deps @!dep-tree})))

(defn read-dep [dep]
  (-read-dep *ctx* dep))

(defn mark-deps! [deps]
  (-mark-deps! *ctx* deps))

