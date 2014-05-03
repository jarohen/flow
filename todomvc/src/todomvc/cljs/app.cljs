(ns todomvc.cljs.app
  (:require [cljs.core.async :as a]
            [todomvc.cljs.todomvc-widget :refer [make-todomvc]]
            [todomvc.cljs.todomvc-model :as model]
            [dommy.core :as d])
  (:require-macros [dommy.macros :refer [sel1]]
                   [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(defn test-todos []
  (->> (for [x (range 5)]
         [x {:caption (str "Test todo " x)}])
       (into {})))

(defn run-benchmark! [!todos]
  (reset! !todos {})
  (go
    (let [els 100]
      (dotimes [i els]
        (swap! !todos
               assoc i {:caption (str "test" i), :done? false}))

      (dotimes [i els]
        (swap! !todos
               assoc-in [i :done?] true))

      (dotimes [i els]
        (swap! !todos
               dissoc i))

      (swap! !todos
             assoc els {:caption (str "test" els), :done? false}))))

(set! (.-onload js/window)
      (fn []
        (let [!todos (atom (test-todos))
              events-ch (doto (a/chan)
                          (model/watch-events! !todos))]

          (d/replace-contents! (sel1 :#content) (make-todomvc !todos events-ch))

          #_(run-benchmark! !todos))))
