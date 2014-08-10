(ns flow.counter.ui.app
  (:require [flow.core :as f :include-macros true]
            [clojure.string :as s]
            simple-brepl.client))

(enable-console-print!)

(set! (.-onload js/window)
      (fn []
        (f/root js/document.body
          (f/el
            [:p "Hello world!"]))))


