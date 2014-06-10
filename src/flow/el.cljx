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
(defn el<< [{:keys [$container]} el-stream]
  (let [el-ch (stream-ch el-stream (a/chan) #(a/sliding-buffer 1))]
    
    (go-loop []
      (when-let [$el (a/<! el-ch)]
        (d/replace-contents! $container (unwrap-nil $el))
        (recur)))
    
    $container))

