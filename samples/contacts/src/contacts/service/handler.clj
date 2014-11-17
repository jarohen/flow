(ns contacts.service.handler
  (:require [compojure.core :refer [routes GET]]
            [compojure.handler :refer [api]]
            [compojure.route :refer [resources]]
            [clojure.java.io :as io]
            [frodo.web :refer [App]]
            [hiccup.page :refer [html5 include-css include-js]]
            [ring.util.response :refer [response]]
            [simple-brepl.service :refer [brepl-js]]))

(defn page-frame []
  (html5
   [:head
    [:title "Flow - Contacts Demo"]

    [:script (brepl-js)]
    
    (include-js "//cdnjs.cloudflare.com/ajax/libs/jquery/2.0.3/jquery.min.js")
    (include-js "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js")
    (include-css "//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css")

    (if-let [cljs-base (io/resource "js/goog/base.js")]
      (list (include-js "/js/goog/base.js")
            (include-js "/js/contacts.js")
            [:script "goog.require('contacts.ui.app');"])
      
      (include-js "/js/contacts.js"))]
   [:body]))

(defn app-routes []
  (routes
    (GET "/" [] (response (page-frame)))
    (resources "/js" {:root "js"})))

(def app
  (reify App
    (start! [_]
      {:frodo/handler (-> (app-routes)
                          api)})
    (stop! [_ system])))
