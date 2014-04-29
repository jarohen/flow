(ns flow.el
  (:require #+clj [clojure.core.async :as a :refer [go go-loop alt!]]
            #+cljs [cljs.core.async :as a]
            #+cljs [dommy.core :as d]
            [clojure.data :refer [diff]]
            [clojure.set :as set]
            [flow.stream :refer [nil-sentinel stream-ch]])

  #+cljs
  (:require-macros [dommy.macros :refer [node sel1]]
                   [cljs.core.async.macros :refer [go go-loop alt!]]))

#+cljs
(defn- new-container []
  (node [:div {:style {:display "inline"}}]))

#+cljs
(defn- update-els! [$container old-els new-els]
  (let [old-ids (doto (:flow/ids (meta old-els)) prn)
        new-ids (doto (:flow/ids (meta new-els)) prn)
        old-id->el (zipmap old-ids old-els)
        new-id->el (zipmap new-ids new-els)

        deleted-ids (set/difference (set old-ids) (set new-ids))
        _ (doseq [id deleted-ids]
            (d/remove! (doto (old-id->el id) prn)))]
    
    (when-not (seq old-els)
      (d/replace-contents! $container new-els))))

;; TODO there's some doubling up going on here which is causing some
;; strange effects - think it's the very first time through for<<

#+cljs
(defn el<< [el-stream]
  (let [$container (new-container)
        el-ch (stream-ch el-stream (a/chan) #(a/sliding-buffer 1))]
    
    (go-loop [old-els nil]
      (when-let [$el (a/<! el-ch)]
        (if (:flow/ids (meta $el))
          (let [els $el]
            (prn (map #(.-randId %) els))
            (update-els! $container old-els els)
            (when (= 3 (count els))
              (d/replace-contents! $container "hell no!"))
            (recur els))
          
          (let [$el (if (= nil-sentinel $el)
                      nil
                      $el)]
            (d/replace-contents! $container $el)
            (recur nil)))))
    
    $container))


