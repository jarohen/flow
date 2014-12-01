(ns contacts.ui.app
  (:require [clojure.string :as s]
            [cljs.core.async :as a]
            [goog.events.KeyCodes :as kc]
            [contacts.formatter :as cf]
            [flow.core :as f :include-macros true])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(defn contact-widget [contact event-ch]
  (f/el
    [:li
     [:span (cf/display-name contact)]
     [:button.btn.btn-link {::f/on {:click #(a/put! event-ch {:type :delete
                                                              :contact contact})}}
      "[delete]"]]))

(defn contact-list-widget [!contacts event-ch]
  (f/el
    [:div
     [:h1 "Contact List:"]
     [:ul
      (for [contact (->> (<< !contacts)
                         (f/keyed-by (juxt :first :last))
                         (sort-by :last))]
        [contact-widget contact event-ch])]

     [:div
      (let [contact-count (count (<< !contacts))]
        [:span contact-count " "
         (if (= contact-count 1)
           "contact"
           "contacts")
         "."])]]))

(defn new-contact-box [event-ch]
  (let [!new-contact-name (atom nil)]
    (f/el
      [:div {::f/style {:margin-top "1em"}}
       [:input#new-contact.form-control {:type "text"
                                         :placeholder "New Contact"
                                         ::f/style {:display :inline
                                                    :width "15em"}
                                         :autofocus true
                                         :value (<< !new-contact-name)
                                         ::f/on {:keyup (juxt (f/bind-value! !new-contact-name)
                                                              (fn [e]
                                                                (when (= kc/ENTER (.-keyCode e))
                                                                  (a/put! event-ch {:type :create
                                                                                    :name @!new-contact-name})
                                                                  (reset! !new-contact-name ""))))}}]])))

(defn handle-events! [event-ch !contacts]
  (go-loop []
    (when-let [{:keys [type] :as event} (a/<! event-ch)]
      (case type
        :create
        (swap! !contacts conj (cf/parse-contact (:name event)))

        :delete
        (swap! !contacts disj (:contact event)))
      (recur))))

(def test-contacts
  #{{:first "Ben" :last "Bitdiddle" :email "benb@mit.edu"}
    {:first "Alyssa" :middle-initial "P" :last "Hacker" :email "aphacker@mit.edu"}
    {:first "Eva" :middle "Lu" :last "Ator" :email "eval@mit.edu"}
    {:first "Louis" :last "Reasoner" :email "prolog@mit.edu"}
    {:first "Cy" :middle-initial "D" :last "Effect" :email "bugs@mit.edu"}
    {:first "Lem" :middle-initial "E" :last "Tweakit" :email "morebugs@mit.edu"}})

(set! (.-onload js/window)
      (fn []
        (let [!contacts (atom test-contacts)
              event-ch (doto (a/chan)
                         (handle-events! !contacts))]
          (f/root js/document.body
            (f/el
              [:div.container
               [contact-list-widget !contacts event-ch]
               [new-contact-box event-ch]])))))
