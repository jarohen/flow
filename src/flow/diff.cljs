(ns flow.diff
  (:require [clojure.set :as set]))

(def diff-threshold
  ;; 5 is a good value here for a lot of use-cases. Inserting/Deleting
  ;; doesn't count as it's cheap anyway, moving one element somewhere
  ;; else in the array is always less than 2 (top -> bottom =
  ;; 2). Shuffling/reversing have a value proportional to the square
  ;; of the size which is greater than 5 for n > ~15
  5.0)

(defn kept-ids [x1 x2]
  (let [kept? (set/intersection (set x1)
                                (set x2))]
    [(filter kept? x1)
     (filter kept? x2)
     kept?]))

(defn indices [m]
  (->> (map vector m (range))
       (into {})))

(defn to-diff? [x1 x2]
  (let [[x1-kept x2-kept ids] (kept-ids x1 x2)
        [x1-indices x2-indices] (map indices [x1-kept x2-kept])]
    (> (* diff-threshold (count ids))
       (->> ids
            (map (juxt x1-indices x2-indices))
            (map #(js/Math.abs (apply - %)))
            (apply +)))))
