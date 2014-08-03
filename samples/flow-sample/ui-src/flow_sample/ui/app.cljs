(ns flow-sample.ui.app
  (:require [flow.core :as f :include-macros true]
            [cljs.core.async :as a]
            [garden.color :as c]
            simple-brepl.client)
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(defn rand-color []
  (c/rgb->hex (c/rgb (rand-int 256) (rand-int 256) (rand-int 256))))

(defn render-svg [!colors]
  #_(f/el
      [:div {::f/style {:margin "1em 0"
                        :color "#000"}}
       [:h3 "And now for an SVG example:"]

       (let [{:keys [primary secondary]} (<< !colors)]
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

(defn render-colour-picker [!color]
  (f/el
    [:p
     [:input {:type "color"
              :value (<< !color)
              ::f/on {:change (f/bind-value! !color)}}]]))

(defn update-random-numbers! [!random-numbers update-numbers-ch]
  (do
    (reset! !random-numbers (for [idx (range 5)]
                              {:id idx
                               :num (rand-int 1000)}))
  
    (go-loop []
      (a/<! update-numbers-ch)
      (swap! !random-numbers (fn [random-numbers]
                               (for [{:keys [id num] :as rn} random-numbers]
                                 {:id id
                                  :num (rand-int 1000)})))

      (recur))

    !random-numbers))

(defn update-colors! [!colors change-colors-ch]
  (go-loop []
    (a/<! change-colors-ch)
    (reset! !colors {:primary (rand-color)
                     :secondary (rand-color)})
    (recur))

  !colors)

(set! (.-onload js/window)
      (fn []
        (let [change-colors-ch (a/chan)
              update-numbers-ch (a/chan)

              !colors (doto (atom {:primary (rand-color)
                                   :secondary (rand-color)})
                        (update-colors! change-colors-ch))
              !show-heading? (atom true)
              !heading (atom "Hello world!")

              !random-numbers (doto (atom nil)
                                (update-random-numbers! update-numbers-ch))
              !filter (atom "even")]

          (def !foo-colors !colors)
          (def !foo-show-heading? !show-heading?)
          (def !foo-heading !foo-heading)
          (def !foo-random-numbers !random-numbers)

          (f/root js/document.body
            (f/el
              [:div#test.container.blah {::f/classes ["abc"
                                                      (when (= (:primary (<< !colors)) "#000")
                                                        "black")]
                                                                                      
                                         ::f/style {:margin-top "2em"}
                                                 
                                         :data-test "foo"
                                                 
                                         :data-is-black (boolean (= (:primary (<< !colors)) "#000"))}

               (when (<< !show-heading?)
                 [:div
                  [:h1 {::f/style {:color (:secondary (<< !colors))
                                   :padding "0.5em"
                                   :background-color (:primary (<< !colors))}}
                   (<< !heading)]])
               
               [:p.copy {::f/style {:text-align :center
                                    :color (:secondary (<< !colors))}}
                "If this works, " [:strong "I'll be very happy :)"]]

               [:button.btn.btn-default {::f/style {:margin-right "1em"}
                                         ::f/on {:click #(js/alert (str "Hello! " (pr-str (<< !colors))))}}
                "Click me!"]

               [:button.btn.btn-default {::f/style {:margin-right "1em"}
                                         ::f/on {:click (fn [e] (a/put! change-colors-ch :change!))}}
                "Change colours!"]

               [:button.btn.btn-default {::f/style {:margin-right "1em"}
                                         ::f/on {:click #(swap! !show-heading? not)}}
                "Show/Hide heading!"]
               ]

              #_(let [{:keys [primary secondary]} (<< !colors)]
                  [:div#test.container.blah {::f/classes ["abc"
                                                          (when (= primary "#000")
                                                            "black")]
                                                                                      
                                             ::f/style {:margin-top "2em"}
                                                 
                                             :data-test "foo"
                                                 
                                             :data-is-black (boolean (= primary "#000"))}
               
                   (when (<< !show-heading?)
                     [:div
                      [:h1 {::f/style {:color secondary
                                       :padding "0.5em"
                                       :background-color primary}}
                       (<< !heading)]])

                   [:p.copy {::f/style {:text-align :center
                                        :color (:secondary (<< !colors))}}
                    "If this works, " [:strong "I'll be very happy :)"]]

                   [:button.btn.btn-default {::f/style {:margin-right "1em"}
                                             ::f/on {:click #(js/alert "Hello!")}}
                    "Click me!"]

                   [:button.btn.btn-default {::f/style {:margin-right "1em"}
                                             ::f/on {:click (fn [e] (a/put! change-colors-ch :change!))}}
                    "Change colours!"]

                   [:button.btn.btn-default {::f/style {:margin-right "1em"}
                                             ::f/on {:click #(swap! !show-heading? not)}}
                    "Show/Hide heading!"]

                   [:button.btn.btn-default {::f/style {:margin-right "1em"}
                                             ::f/on {:click (fn [e] (a/put! update-numbers-ch :change!))}}
                    "Update numbers!"]

                   [:div {::f/style {:margin "1em 0"
                                     :color "#000"}}
                    [:h3 "And now for a 'for' example:"]

                    (let [random-numbers (<< !random-numbers)
                          selected-filter (<< !filter)]
                      [:div
                       [:p "!random-numbers: " random-numbers]
                       [:p "filter: " (case selected-filter
                                        "even" (let [even-count (count (filter (comp even? :num) random-numbers))]
                                                 [:span "Only the " even-count
                                                  " even" (when (not= 1 even-count)
                                                            "s")])
                                        "odd" (let [odd-count (count (filter (comp odd? :num) random-numbers))]
                                                [:span "Only the " odd-count
                                                 " odd" (when (not= 1 odd-count)
                                                          "s")])
                                        (let [all-count (count random-numbers)]
                                          [:span "All " all-count]))]
                   
                       [:ul {::f/style {:margin-top "1em"}}
                        (for [{:keys [num]} (->> random-numbers
                                                 (filter (comp (case selected-filter
                                                                 "even" even?
                                                                 "odd" odd?
                                                                 "all" identity)
                                                               :num))
                                                 (sort-by :num))]
                          [:li num])]])]

                   [:div
                    [:select.form-control {::f/on {:change (f/bind-value! !filter)}
                                           :value (<< !filter)
                                           ::f/style {:width "6em"}}
                     [:option {:value "odd"}
                      "Odd"]
                     [:option {:value "even"}
                      "Even"]
                     [:option {:value "all"}
                      "All"]]]

                   [:div
                    [:h3 "Pick your colours:"]

                    [:p "Primary:" (render-colour-picker (!<< primary))]
                  
                    [:p "Secondary:" (render-colour-picker (!<< secondary))]]
                 
                   (render-svg !colors)]))))))

