(ns flow.deps-test
  (:require [flow.deps :as fd :refer :all]
            [flow.lenses.common :refer [Lens]]
            [clojure.test :refer :all]))

(defn mock-lens [!atom path]
  (reify Lens
    (-!state [_] !atom)
    (-path [_] path)))

(def nested-map
  {:a {:e 15, :b 45}
   :c {:d 54}})

(deftest marking-deps-in-the-tree
  (let [!atom (atom nested-map)]
    (is (= (mark-dep {} !atom nested-map)
           {!atom {:flow.deps/value nested-map}}))

    (is (= (mark-dep {} (mock-lens !atom [:a :b]) 45)
           {!atom {:a {:b {::fd/value 45}}}}))

    (is (= (-> {}
               (mark-dep (mock-lens !atom [:a :b]) 45)
               (mark-dep (mock-lens !atom [:c :d]) 54)
               (mark-dep (mock-lens !atom [:a]) {:b 45 :e 15}))
           {!atom {:c {:d {:flow.deps/value 54}},
                   :a {:flow.deps/value {:e 15, :b 45}}}}))))

(deftest deps-unchanged-test
  (let [!atom (atom nested-map)
        deps-tree (-> {}
                      (mark-dep (mock-lens !atom [:a :b]) (get-in @!atom [:a :b]))
                      (mark-dep (mock-lens !atom [:c :d]) (get-in @!atom [:c :d]))
                      (mark-dep (mock-lens !atom [:a]) (get-in @!atom [:a])))]
    
    (is (true? (deps-unchanged? deps-tree)))

    (swap! !atom assoc :f {:g 12})

    (is (true? (deps-unchanged? deps-tree)))

    (swap! !atom assoc-in [:c :e] 29)

    (is (true? (deps-unchanged? deps-tree)))

    (swap! !atom assoc-in [:a :e] 25)

    (is (false? (deps-unchanged? deps-tree)))))


