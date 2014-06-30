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

(comment
  (f/el
   [:div#test.container.blah {::f/style {:color (<<! !color)}
                              :data-test "foo"}
    [:h1 "Hello world!"]
    [:p.copy {::f/style {:text-align :right}}
     "If this works, " [:strong "I'll be very happy :)"]]])
  
  (let [color (<<! !color)
        text-decor (<<! !text-decor)]
    [:div {::f/style {:color (case color
                               :pink "#424"
                               :green "#080"
                               :blue "#238")

                      :text-decoration text-decor}}])

  {:dynamic-syms {'color 'color_80124}
   :state-sym 'state_8932
   :updated-sym 'updated-sym_8933}
  
  {:init nil
   :deps #{'color_80124}
   :on-update (let ['color (get-in state [:values 'color_80214])]
                (case color
                  :pink "#424"
                  :green "#080"
                  :blue "#238"))}

  {:init nil
   :deps #{'color_80124 'text-decor_19381}
   :atom-deps #{}
   :on-update [(when (contains? #{'color_80124 :all} updated-sym_8933)
                 (let [color (get-in state [:values 'color_80214])]
                   (case color
                     :pink "#424"
                     :green "#080"
                     :blue "#238")))

               (when (contains? #{'text-decor_19381 :all} updated-sym)
                 )]}


  {:init `[(aset ~elem-sym 'style 'color ~v)]})


(comment
  (let [val (<<! !atom)
        !atom (!>> val)
        ]))
