(ns flow.todomvc.service.handler
  (:require [clojure.java.io :as io]
            [compojure.core :refer [routes GET]]
            [compojure.handler :refer [api]]
            [compojure.route :refer [resources]]
            [hiccup.page :refer [html5 include-css include-js]]
            [ring.util.response :refer [response]]
            [simple-brepl.service :refer [brepl-js]]))

(defn page-frame []
  (html5
   [:head
    [:title "TodoMVC - A sample Flow application"]
    [:meta {:charset "utf-8"}]

    [:script (brepl-js)]
    
    (if-let [cljs-base (io/resource "js/goog/base.js")]
      (list (include-js "/js/goog/base.js")
            (include-js "/js/todomvc.js")
            [:script "goog.require('flow.todomvc.ui.app');"])
      
      (include-js "/js/todomvc.js"))
    
    (include-css "/todomvc/todomvc.css")]
   
   [:body
    [:div#content]]))

(defn app-routes []
  (routes
    (GET "/" [] (response (page-frame)))
    (resources "/js" {:root "js"})
    (resources "/img" {:root "img"})
    (resources "/todomvc" {:root "todomvc"})))

(defn app []
  (-> (app-routes)
      api))
