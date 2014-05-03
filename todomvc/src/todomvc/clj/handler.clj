(ns todomvc.clj.handler
  (:require [ring.util.response :refer [response content-type]]
            [compojure.core :refer [routes GET]]
            [compojure.route :refer [resources]]
            [compojure.handler :refer [api]]
            [hiccup.page :refer [html5 include-css include-js]]
            [clojure.java.io :as io]))

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
