(ns contacts.cljs.app
  (:require [cljs.core.async :as a]
            [contacts.cljx.formatter :as f]
            [dommy.core :as d]
            [goog.events.KeyCodes :as kc]
            [flow.core :refer [el<<]]
            frodo.brepl)
  (:require-macros [dommy.macros :refer [node sel1]]
                   [cljs.core.async.macros :refer [go-loop]]
                   [flow.core :refer [let<<]]))

(enable-console-print!)

(defn with-delete-handler! [$button contact event-ch]
  (d/listen! $button :click
    #(a/put! event-ch {:type :delete
                       :contact contact})))

(defn contact-widget [contact event-ch]
  (node
   [:li
    [:span (f/display-name contact)]
    (-> (node [:button.btn.btn-link "[delete]"])
        (with-delete-handler! contact event-ch))]))

(defn contact-list-widget [!contacts event-ch]
  (node
   [:div
    [:h1 "Contact List:"]
    [:ul
     (comment
       (el<<
        (for<< [contact << (let<< [contacts !contacts]
                             (sort-by :last contacts))]
          (contact-widget contact event-ch))))
     
     (el<<
      (let<< [contacts << !contacts]
        (node
         [:div
          (for [contact (sort-by :last contacts)]
            (contact-widget contact event-ch))])))]]))

(defn with-submit-handler! [$new-contact-box event-ch]
  (d/listen! $new-contact-box :keyup
    (fn [e]
      (when (= kc/ENTER (.-keyCode e))
        (a/put! event-ch {:type :create
                          :name (d/value $new-contact-box)})
        (d/set-value! $new-contact-box nil)))))

(defn new-contact-box [event-ch]
  (-> (node [:input#new-contact.form-control
             {:type "text"
              :placeholder "New Contact"
              :autofocus true}])
      (with-submit-handler! event-ch)))

(defn handle-events! [event-ch !contacts]
  (go-loop []
    (when-let [{:keys [type] :as event} (a/<! event-ch)]
      (case type
        :create
        (swap! !contacts conj (f/parse-contact (:name event)))

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
          (d/replace-contents! (sel1 :#content)
                               (node
                                [:div.container
                                 (contact-list-widget !contacts event-ch)
                                 (new-contact-box event-ch)])))))


