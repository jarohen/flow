(ns flow.diff
  (:require [clojure.data :as cd]
            [clojure.set :as set]))

(defn updated-keys [m1 m2]
  ;; we assume m1 and m2 have the same keys - a safe assumption because the bindings don't change
  (->> (keys m2)
       (filter #(not= (get m1 %)
                      (get m2 %)))
       set))

(defn vector-diff [old-ids new-ids]
  (let [[removed added _] (cd/diff (set old-ids) (set new-ids))]
    {:added added
     :removed removed
     :diff (loop [res []
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
                     more-news)))}))


(comment
  (time (vector-diff [0 1 2 3 4 5 6 8 9 10 7] [0 8 9 1 2 4 5 6 10 11])))
