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

(defn do-diff [old-ids new-ids]
  (loop [res []
         displaced-olds []
         displaced-news []
         [old-id & more-olds :as old-ids] old-ids
         [new-id & more-news :as new-ids] new-ids]

    (assert (not (some nil? new-ids)) "Flow: can't have nil ids")
    
    (comment
      (prn)
      (prn res)
      (prn old-ids)
      (prn new-ids)
      (prn displaced-olds)
      (prn displaced-news))
    
    (cond
     (and (empty? old-ids)
          (empty? new-ids))
     (concat res
             (for [displaced-new displaced-news]
               [:moved-in displaced-new])
             (for [displaced-old displaced-olds]
               [:moved-out displaced-old]))

     (contains? (set displaced-olds) new-id)
     (let [[moved-out-olds still-displaced-olds] (split-with #(not= new-id %) displaced-olds)]
       (recur (concat res
                      (for [displaced-new displaced-news]
                        [:moved-in displaced-new])
                      (for [moved-out-old moved-out-olds]
                        [:moved-out moved-out-old])
                      [[:kept new-id]])
              (rest still-displaced-olds)
              []
              old-ids
              more-news))

     (contains? (set displaced-news) old-id)
     (let [[moved-in-news still-displaced-news] (split-with #(not= old-id %) displaced-news)]
       (recur (concat res
                      (for [displaced-old displaced-olds]
                        [:moved-out displaced-old])
                      (for [moved-in-new moved-in-news]
                        [:moved-in moved-in-new])
                      [[:kept old-id]])
              []
              (rest still-displaced-news)
              more-olds
              new-ids))

     (= old-id new-id)
     (recur (concat res
                    (for [displaced-new displaced-news]
                      [:moved-in displaced-new])
                    (for [displaced-old displaced-olds]
                      [:moved-out displaced-old])
                    [[:kept new-id]])
            []
            []
            more-olds
            more-news)
     
     (not= old-id new-id)
     (recur res
            (concat displaced-olds
                    (when (seq old-ids)
                      [old-id]))
            (concat displaced-news
                    (when (seq new-ids)
                      [new-id]))
            more-olds
            more-news))))

(defn vector-diff [old-ids new-ids]
  (if (to-diff? old-ids new-ids)
    (do-diff old-ids new-ids)

    (concat (for [old-id old-ids]
              [:moved-out old-id])
            (for [new-id new-ids]
              [:moved-in new-id]))))
