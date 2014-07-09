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
              change-colors-ch (a/chan)
              handle-click! (fn []
                              (a/put! change-colors-ch :change!))]

          (def !foo-colors !colors)
          (def !foo-show-heading? !show-heading?)
          (def !foo-heading !heading)
          
          (f/root js/document.body
                  (f/el
                    [:div#test.container.blah {::f/classes ["abc"
                                                            (when (= (:primary (<<! !colors)) "#000")
                                                              "black")]
                                                 
                                               ::f/style {:color (:primary (<<! !colors))}
                                                 
                                               :data-test "foo"
                                                 
                                               :data-is-black (boolean (= (:primary (<<! !colors)) "#000"))}

                     (let [show-heading? (<<! !show-heading?)]
                       [:div
                        (when show-heading?
                          [:h1 {::f/style {:color (:secondary (<<! !colors))
                                           :padding "0.5em"
                                           :background-color (:primary (<<! !colors))}}
                           (<<! !heading)])

                        [:p "Show heading is: " show-heading?]])
                       
                     [:p.copy {::f/style {:text-align :right
                                          :color (:secondary (<<! !colors))}}
                      "If this works, " [:strong "I'll be very happy :)"]]

                     [:button.btn.btn-default {::f/on {:click (fn [e] (a/put! change-colors-ch :change!))}}
                      "Change colours!"]

                     [:button.btn.btn-default {::f/on {:click #(swap! !show-heading? not)}}
                      "Show/Hide heading!"]]




                    
                    #_(let [primary (:primary (<<! !colors))]
                        [:div#test.container.blah {::f/classes ["abc"
                                                                (when (= primary "#000")
                                                                  "black")]
                                                 
                                                   ::f/style {:color primary}
                                                 
                                                   :data-test "foo"
                                                 
                                                   :data-is-black (boolean (= primary "#000"))}

                         (when (<<! !show-heading?)
                           [:h1 {::f/style {:color (:secondary (<<! !colors))
                                            :padding "0.5em"
                                            :background-color primary}
                                 ::f/on {:click (fn [e]
                                                  (a/put! change-colors-ch :change!))}}
                            (<<! !heading)])
                       
                         (if (= 2 (+ 1 1))
                           [:p.copy {::f/style {:text-align :right
                                                :color (:secondary (<<! !colors))}}
                          
                            "If this works, " [:strong "I'll be very happy :)"]]

                           [:p "Maths always let me down..."])

                         [:button.btn.btn-default {::f/on {:click (fn [e] (a/put! change-colors-ch :change!))}}
                          "Change colours!"]

                         [:button.btn.btn-default {::f/on {:click #(swap! !show-heading? not)}}
                          (if (<<! !show-heading?) 
                            "Hide heading!"
                            "Show heading!")]])))

          (go-loop []
            (a/<! change-colors-ch)
            (reset! !foo-colors {:primary (rand-color)
                                 :secondary (rand-color)})
            (recur)))))

