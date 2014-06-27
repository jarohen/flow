(ns flow-sample.cljs.app
  (:require [flow.core :as f :include-macros true])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(set! (.-onload js/window)
      (fn []
        (f/root js/document.body
                (f/el
                 [:div.container {::f/style {:color "#439"}
                                  ::f/classes ["abc"
                                               (when (zero? (rand-int 3))
                                                 "blah")]}
                  [:h1 "Hello world!"]
                  
                  (when-not (zero? (rand-int 5))
                    [:p.copy {::f/style {:float :right}}
                     "If I can get this working tonight, "
                     [:strong "I'll be very happy :)"]])]))))


