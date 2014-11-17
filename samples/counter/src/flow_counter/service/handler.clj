(ns flow-counter.service.handler
  (:require [frodo.web :refer [App]]
            [clojure.java.io :as io]
            [compojure.core :refer [routes GET]]
            [compojure.handler :refer [api]]
            [compojure.route :refer [resources]]
            [hiccup.page :refer [html5 include-css include-js]]
            [ring.util.response :refer [response]]
            [simple-brepl.service :refer [brepl-js]]))

(defn page-frame []
  (html5
   [:head
    [:title "Flow Counter Tutorial"]

    [:script (brepl-js)]
    
    (include-js "//cdnjs.cloudflare.com/ajax/libs/jquery/2.0.3/jquery.min.js")
    (include-js "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js")
    (include-css "//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css")

    (if-let [cljs-base (io/resource "js/goog/base.js")]
      (list (include-js "/js/goog/base.js")
            (include-js "/js/counter.js")
            [:script "goog.require('flow_counter.ui.app');"])
      
      (include-js "/js/counter.js"))]
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
