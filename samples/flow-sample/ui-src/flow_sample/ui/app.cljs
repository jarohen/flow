(ns flow-sample.ui.app
  (:require [flow.core :as f :include-macros true]
            [cljs.core.async :as a]
            simple-brepl.client)
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(defn rand-color []
  (str "rgb(" (rand-int 256) "," (rand-int 256) "," (rand-int 256) ")"))

(defn render-svg [!colors]
  #_(f/el
      [:div {::f/style {:margin "1em 0"
                        :color "#000"}}
       [:h3 "And now for an SVG example:"]
       (let [{:keys [primary secondary]} (!<< !colors)]
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
                               :fill primary}}]])]))

(set! (.-onload js/window)
      (fn []
        (let [!colors (atom {:primary "#427"
                             :secondary "#983"})
              !show-heading? (atom true)
              !heading (atom "Hello world!")
              change-colors-ch (a/chan)
              update-numbers-ch (a/chan)

              !random-numbers (atom (for [idx (range 5)]
                                      {:id idx
                                       :num (rand-int 1000)}))
              !input-value (atom 2)]

          (def !foo-colors !colors)
          (def !foo-show-heading? !show-heading?)
          (def !foo-heading !foo-heading)
          (def !foo-random-numbers !random-numbers)

          (go-loop []
            (a/<! update-numbers-ch)
            (swap! !random-numbers (fn [random-numbers]
                                     (for [{:keys [id num] :as rn} random-numbers]
                                       {:id id
                                        :num (rand-int 1000)})))

            (recur))
          
          (f/root js/document.body
            (f/el
              [:div.container {::f/style {:margin-top "2em"}}

               
               
               (when (!<< !show-heading?)
                 [:div
                  (let [{:keys [primary secondary]} (!<< !colors)]
                    [:h1 {::f/style {:color secondary
                                     :padding "0.5em"
                                     :background-color primary}}
                     (!<< !heading)])])

               [:p.copy {::f/style {:text-align :center
                                    :color (:secondary (!<< !colors))}}
                "If this works, " [:strong "I'll be very happy :)"]]

               [:div
                [:button.btn.btn-default {::f/style {:margin-right "1em"}
                                          ::f/on {:click #(js/alert "Hello!")}}
                 "Click me!"]

                [:button.btn.btn-default {::f/style {:margin-right "1em"}
                                          ::f/on {:click (fn [e] (a/put! change-colors-ch :change!))}}
                 "Change colours!"]

                [:button.btn.btn-default {::f/style {:margin-right "1em"}
                                          ::f/on {:click #(swap! !show-heading? not)}}
                 "Show/Hide heading!"]]]
              
              #_(let [{:keys [primary secondary]} (!<< !colors)]
                  [:div#test.container.blah {::f/classes ["abc"
                                                          (when (= primary "#000")
                                                            "black")]
                                                 
                                             :data-test "foo"
                                                 
                                             :data-is-black (boolean (= primary "#000"))}

                   (let [show-heading? (!<< !show-heading?)]
                     [:div
                      (when show-heading?
                        [:h1 {::f/style {:color secondary
                                         :padding "0.5em"
                                         :background-color primary}}
                         (!<< !heading)])])

                   [:p.copy {::f/style {:text-align :center
                                        :color secondary}}
                    "If this works, " [:strong "I'll be very happy :)"]]

                   [:button.btn.btn-default {::f/style {:margin-right "1em"}
                                             ::f/on {:click (fn [e] (a/put! change-colors-ch :change!))}}
                    "Change colours!"]

                   [:button.btn.btn-default {::f/style {:margin-right "1em"}
                                             ::f/on {:click (fn [e] (a/put! update-numbers-ch :change!))}}
                    "Update numbers!"]

                   

                   [:div {::f/style {:margin "1em 0"
                                     :color "#000"}}
                    [:h3 "And now for a 'for' example:"]

                    (let [random-numbers (!<< !random-numbers)]
                      [:div
                       [:div "!random-numbers: " random-numbers]
                     
                       [:ul {::f/style {:margin-top "1em"}}
                        (for [{:keys [num]} (->> random-numbers
                                                 (filter (comp even? :num))
                                                 (sort-by :num))]
                          [:li num])]])]

                   [:div
                    [:div "Input value:" (!<< !input-value)]

                    [:select {::f/on {:change (f/bind-value! !input-value)}
                              :value (!<< !input-value)}
                     [:option {:value 1}
                      "Option 1"]
                     [:option {:value 2}
                      "Option 2"]]]

                   (render-svg !colors)])))

          (go-loop []
            (a/<! change-colors-ch)
            (reset! !foo-colors {:primary (rand-color)
                                 :secondary (rand-color)})
            (recur)))))

