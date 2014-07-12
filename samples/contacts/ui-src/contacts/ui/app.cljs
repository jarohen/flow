(ns contacts.ui.app
  (:require [clojure.string :as s]
            [flow.core :as f :include-macros true]
            simple-brepl.client))

(enable-console-print!)

(set! (.-onload js/window)
      (fn []
        (f/root js/document.body
          (f/el
            [:h1 "Hello world!"]))))


