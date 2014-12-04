(ns flow-counter.ui.app
  (:require [flow.core :as f :include-macros true]
            simple-brepl.client))

(enable-console-print!)

(def !counter
  (atom 0))

(set! (.-onload js/window)
      (fn []
        (f/root js/document.body
          (f/el
            [:div.container {::f/style {:margin-top "2em"}}
             (list
              [:p "The value of the counter is " (<< !counter)]
             
              [:p [:button.btn.btn-default {::f/on {:click #(swap! !counter inc)}}
                   "Increment me!"]])]))))


