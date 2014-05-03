(ns contacts.clj.handler
  (:require [ring.util.response :refer [response content-type]]
            [compojure.core :refer [routes GET]]
            [compojure.route :refer [resources]]
            [compojure.handler :refer [api]]
            [hiccup.page :refer [html5 include-css include-js]]
            [frodo.brepl :refer [brepl-js]]
            [contacts.clj.css :as css]))

(defn page-frame []
  (html5
   [:head
    [:title "contacts - CLJS Single Page Web Application"]
    [:script (brepl-js)]
    (include-js "//cdnjs.cloudflare.com/ajax/libs/jquery/2.0.3/jquery.min.js")
    (include-js "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js")
    (include-css "//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css")

    (include-js "/js/contacts.js")
    (include-css "/css/main.css")]
   [:body
    [:div#content]]))

(defn app-routes []
  (routes
    (GET "/" [] (response (page-frame)))
    (resources "/js" {:root "js"})
    (GET "/css/main.css" [] (-> (response css/main-css)
                                (content-type "text/css")))))

(defn app []
  (-> (app-routes)
      api))
