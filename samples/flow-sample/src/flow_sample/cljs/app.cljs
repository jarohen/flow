(ns flow-sample.cljs.app
  (:require [flow.core :as f :include-macros true])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(set! (.-onload js/window)
      (fn []
        (f/root js/document.body
                (f/el
                 [:div#test.container.blah {::f/classes ["abc"]
                                            :data-test "foo"}
                  [:h1 "Hello world!"]
                  [:p.copy {::f/style {:text-align :right}}
                   "If this works, " [:strong "I'll be very happy :)"]]]))))
