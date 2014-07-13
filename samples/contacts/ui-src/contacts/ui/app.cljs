(ns contacts.ui.app
  (:require [clojure.string :as s]
            [contacts.formatter :as cf]
            [flow.core :as f :include-macros true]
            simple-brepl.client))

(enable-console-print!)

(set! (.-onload js/window)
      (fn []
        (let [!contacts (atom
                         [{:first "Kate", :last "Reed"}
                          {:first "James", :last "Henderson"}])]
          
          (f/root js/document.body
            (f/el
              [:div.container
               [:h1 "My address book"]
               [:ul
                (for [contact (<<! !contacts)]
                  [:li
                   (cf/display-name contact)
                   #_[:button.btn.btn-danger {::f/on {:click (let [!contact (!>> contact)]
                                                               #(println "deleting" (pr-str @!contact)))}
                                              ::f/style {:margin "0.2em 1em"}}
                      "X"]])]])))))
