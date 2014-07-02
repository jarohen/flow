(ns flow-sample.cljs.app
  (:require [flow.core :as f :include-macros true]
            simple-brepl.client)
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(set! (.-onload js/window)
      (fn []
        (let [!colors (atom {:primary "#427"
                             :secondary "#983"})]

          (def !foo-colors !colors)
          
          (f/root js/document.body
                  (f/el
                    [:div#test.container.blah {::f/classes ["abc"
                                                            (when (= (:primary (<<! !colors)) "#000")
                                                              "black")]
                                               ::f/style {:color (:primary (<<! !colors))}
                                               :data-test "foo"
                                               :data-is-black (boolean (= (:primary (<<! !colors)) "#000"))}
                     [:h1 "Hello world!"]
                     [:p.copy {::f/style {:text-align :right
                                          :color (:secondary (<<! !colors))}}
                      "If this works, " [:strong "I'll be very happy :)"]]])))))

