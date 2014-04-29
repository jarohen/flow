(ns flow.el
  (:require #+clj [clojure.core.async :as a :refer [go go-loop alt!]]
            #+cljs [cljs.core.async :as a]
            #+cljs [dommy.core :as d]
            [flow.stream :refer [nil-sentinel stream-ch]])

  #+cljs
  (:require-macros [dommy.macros :refer [node sel1]]
                   [cljs.core.async.macros :refer [go go-loop alt!]]))

#+cljs
(defn- new-container []
  (node [:div {:style {:display "inline"}}]))

#+cljs
(defn el<< [el-stream]
  (let [$container (new-container)
        el-ch (stream-ch el-stream (a/chan) #(a/sliding-buffer 1))]
    
    (go-loop []
      (when-let [$el (a/<! el-ch)]
        (let [$el (if (= nil-sentinel $el)
                    nil
                    $el)]

          (d/replace-contents! $container $el)
          (recur))))
    
    $container))

