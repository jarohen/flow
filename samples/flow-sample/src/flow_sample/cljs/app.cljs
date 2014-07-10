(ns flow-sample.cljs.app
  (:require [flow.core :as f :include-macros true]
            [cljs.core.async :as a]
            simple-brepl.client)
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(defn rand-color []
  (str "rgb(" (rand-int 256) "," (rand-int 256) "," (rand-int 256) ")"))

(set! (.-onload js/window)
      (fn []
        (let [!colors (atom {:primary "#427"
                             :secondary "#983"})
              !show-heading? (atom true)
              !heading (atom "Hello world!")
              change-colors-ch (a/chan)]

          (def !foo-colors !colors)
          (def !foo-show-heading? !show-heading?)
          (def !foo-heading !foo-heading)
          
          (f/root js/document.body
                  (f/el
                    (let [{:keys [primary secondary]} (<<! !colors)]
                      [:div#test.container.blah {::f/classes ["abc"
                                                              (when (= primary "#000")
                                                                "black")]
                                                 
                                                 ::f/style {:color primary}
                                                 
                                                 :data-test "foo"
                                                 
                                                 :data-is-black (boolean (= primary "#000"))}

                       (let [show-heading? (<<! !show-heading?)]
                         [:div
                          (when show-heading?
                            [:h1 {::f/style {:color secondary
                                             :padding "0.5em"
                                             :background-color primary}}
                             (<<! !heading)])])

                       [:p.copy {::f/style {:text-align :center
                                            :color secondary}}
                        "If this works, " [:strong "I'll be very happy :)"]]

                       [:button.btn.btn-default {::f/on {:click (fn [e] (a/put! change-colors-ch :change!))}}
                        "Change colours!"]

                       [:button.btn.btn-default {::f/on {:click #(swap! !show-heading? not)}}
                        "Show/Hide heading!"]

                       [:div {::f/style {:margin "1em"
                                         :color "#000"}}
                        [:h3 "And now for an SVG example:"]

                        [:svg 
                         [:rect {:x 10
                                 :y 10
                                 :height 100
                                 :width 100
                                 ::f/style {:stroke primary
                                            :fill secondary}}]
                         [:circle {:cx 60
                                   :cy 60
                                   :r 40
                                   ::f/style {:stroke secondary
                                              :fill primary}}]]]])))

          (go-loop []
            (a/<! change-colors-ch)
            (reset! !foo-colors {:primary (rand-color)
                                 :secondary (rand-color)})
            (recur)))))

