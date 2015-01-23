(ns flow.dom.diff
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

(defn abs [x]
  #+clj (Math/abs x)
  #+cljs (js/Math.abs x))

(defn to-diff? [x1 x2]
  (let [[x1-kept x2-kept ids] (kept-ids x1 x2)
        [x1-indices x2-indices] (map indices [x1-kept x2-kept])]
    (> (* diff-threshold (count ids))
       (->> ids
            (map (juxt x1-indices x2-indices))
            (map #(abs (apply - %)))
            (apply +)))))

(defn do-diff [olds news]
  (loop [res []
         displaced-olds []
         displaced-news []
         [old-el & more-olds :as olds] olds
         [new-el & more-news :as news] news]

    (cond
      (or (empty? olds) (empty? news))
      (concat res
              (for [displaced-new (concat displaced-news news)]
                [:moved-in displaced-new])
              (for [displaced-old (concat displaced-olds olds)]
                [:moved-out displaced-old]))

      (= old-el new-el)
      (recur (concat res
                     (for [displaced-new displaced-news]
                       [:moved-in displaced-new])
                     (for [displaced-old displaced-olds]
                       [:moved-out displaced-old])
                     [[:kept new-el]])
             []
             []
             more-olds
             more-news)

      (contains? (set displaced-olds) new-el)
      (let [[moved-out-olds still-displaced-olds] (split-with #(not= new-el %) displaced-olds)]
        (recur (concat res
                       (for [displaced-new displaced-news]
                         [:moved-in displaced-new])
                       (for [moved-out-old moved-out-olds]
                         [:moved-out moved-out-old])
                       [[:kept new-el]])
               (rest still-displaced-olds)
               []
               olds
               more-news))

      (contains? (set displaced-news) old-el)
      (let [[moved-in-news still-displaced-news] (split-with #(not= old-el %) displaced-news)]
        (recur (concat res
                       (for [displaced-old displaced-olds]
                         [:moved-out displaced-old])
                       (for [moved-in-new moved-in-news]
                         [:moved-in moved-in-new])
                       [[:kept old-el]])
               []
               (rest still-displaced-news)
               more-olds
               news))

      (not= old-el new-el)
      (recur res
             (concat displaced-olds
                     (when (seq olds)
                       [old-el]))
             (concat displaced-news
                     (when (seq news)
                       [new-el]))
             more-olds
             more-news))))

(defn vector-diff [olds news]
  (if (to-diff? olds news)
    (do-diff olds news)

    (concat (for [old-el olds]
              [:moved-out old-el])
            (for [new-el news]
              [:moved-in new-el]))))
