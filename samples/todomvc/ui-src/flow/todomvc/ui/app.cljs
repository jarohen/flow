(ns flow.todomvc.ui.app
  (:require [flow.core :as f :include-macros true]
            [cljs.core.async :as a]
            [flow.todomvc.ui.todomvc-widget :refer [make-todomvc]]
            [flow.todomvc.ui.todomvc-model :as model]
            simple-brepl.client)
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(defn test-todos []
  (->> (for [x (range 5)]
         [x {:id x
             :caption (str "Test todo " x)}])
       (into {})))

(defn run-benchmark! [!todos]
  (reset! !todos {})
  (go
    (let [els 200]
      (dotimes [i els]
        (swap! !todos
               assoc i {:id i
                        :caption (str "test" i),
                        :done? false}))

      (dotimes [i els]
        (swap! !todos
               assoc-in [i :done?] true))
      
      #_(dotimes [i els]
          (swap! !todos
                 dissoc i)))))

(set! (.-onload js/window)
      (fn []
        (let [!todos (atom (test-todos))
              events-ch (doto (a/chan)
                          (model/watch-events! !todos))]

          (f/root js/document.body
            (make-todomvc !todos events-ch))

          (go
            (a/<! (a/timeout 1000))
            #_(run-benchmark! !todos)))))
