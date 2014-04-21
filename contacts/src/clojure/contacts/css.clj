(ns contacts.css
  (:require [gaka.core :as gaka]))

(def main-css
  (gaka/css
   [:ul
    {:line-height "1.5em"}]
   [:button]
   [:input#new-contact
    {:width "15em"
     :display "inline"
     :margin "0.5em"}]))

