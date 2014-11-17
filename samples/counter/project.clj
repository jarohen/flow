(defproject jarohen/flow.counter ""

  :description "An example 'Counter' application for the Flow tutorial"
  :url "https://github.com/james-henderson/blob/master/sample/counter"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  
  :dependencies [[org.clojure/clojure "1.6.0"]

                 [ring/ring-core "1.2.0"]
                 [compojure "1.1.6"]
                 [hiccup "1.0.5"]

                 [jarohen/flow "0.2.0-beta4"]

                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/clojurescript "0.0-2371"]]

  :plugins [[jarohen/lein-frodo "0.4.1"]
            [jarohen/simple-brepl "0.1.2"]
            [lein-cljsbuild "1.0.3"]
            [lein-pdo "0.1.1"]
            [lein-shell "0.4.0"]]

  :frodo/config-resource "counter-config.edn"

  :resource-paths ["resources" "target/resources"]

  :cljsbuild {:builds {:dev
                       {:source-paths ["ui-src"]
                        :compiler {:output-to "target/resources/js/counter.js"
                                   :output-dir "target/resources/js/"
                                   :optimizations :none
                                   :pretty-print true}}

                       :prod
                       {:source-paths ["ui-src"]
                        :compiler {:output-to "target/resources/js/counter.js"
                                   :optimizations :advanced

                                   :pretty-print true}}}}

  :aliases {"dev" ["do"
                   ["shell" "mkdir" "-p"
                    "target/resources"]
                   ["pdo"
                    ["cljsbuild" "auto" "dev"]
                    "frodo"]]})
