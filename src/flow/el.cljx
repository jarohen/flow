(ns flow.el
  (:require #+clj [clojure.core.async :as a :refer [go go-loop alt!]]
            #+cljs [cljs.core.async :as a]
            #+cljs [dommy.core :as d]
            [clojure.data :refer [diff]]
            [clojure.set :as set]
            [flow.stream :refer [unwrap-nil stream-ch]])

  #+cljs
  (:require-macros [dommy.macros :refer [node sel1]]
                   [cljs.core.async.macros :refer [go go-loop alt!]]))

#+cljs
(defn- new-container []
  (node [:div {:style {:display "inline"}}]))

#+cljs
(defn update-els! [$container old-els new-els]
  (let [[deleted inserted kept] (map set (diff old-els new-els))]
    (if (and (= ((fnil remove #{}) deleted old-els)
                ((fnil remove #{}) inserted new-els))

             (< (+ (count inserted) (count deleted))
                (count kept)))

      (do
        (doseq [$el deleted]
          (d/remove! $el))
        (doseq [[$el $after-el] (->> new-els
                                     (partition-all 2 1)
                                     reverse)
                :when (inserted $el)]
          (if $after-el
            (d/insert-before! $after-el)
            (d/append! $container $el))))
      
      (d/replace-contents! $container new-els))))

#+cljs
(defn el<< [el-stream]
  (let [$container (new-container)
        el-ch (stream-ch el-stream (a/chan) #(a/sliding-buffer 1))]
    
    (go-loop [old-els nil]
      (when-let [$el (a/<! el-ch)]
        (if (seq? $el)
          (do
            (update-els! $container old-els $el)
            (recur $el))
          (do
            (d/replace-contents! $container (unwrap-nil $el))
            (recur nil)))))
    
    $container))
