(ns todomvc.clj.handler
  (:require [compojure.core :refer [routes GET]]
            [compojure.handler :refer [api]]
            [compojure.route :refer [resources]]
            [hiccup.page :refer [html5 include-css include-js]]
            [ring.util.response :refer [response]]))

(defn page-frame []
  (html5
   [:head
    [:title "TodoMVC - A sample Flow application"]
    [:meta {:charset "utf-8"}]

    (include-js "/js/todomvc.js")
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
