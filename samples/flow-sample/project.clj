(defproject jarohen/flow.sample ""

  :description "A sample application to demo the Flow library"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  
  :dependencies [[org.clojure/clojure "1.6.0"]

                 [ring/ring-core "1.2.0"]
                 [compojure "1.1.5"]
                 [hiccup "1.0.4"]

                 [prismatic/dommy "0.1.2"]

                 [org.clojure/core.async "0.1.303.0-886421-alpha"]
                 [org.clojure/clojurescript "0.0-2268"]

                 [jarohen/flow "0.1.0"]]

  :plugins [[jarohen/lein-frodo "0.3.2"]
            [jarohen/simple-brepl "0.1.1"]
            [lein-cljsbuild "1.0.3"]
            [com.keminglabs/cljx "0.4.0"]
            [lein-pdo "0.1.1"]]

  :frodo/config-resource "flow-sample-config.edn"

  :resource-paths ["resources" "target/resources"]

  :cljsbuild {:builds {:dev
                       {:source-paths ["ui-src"
                                       "checkouts/flow/src"
                                       "checkouts/flow/target/generated/clj"
                                       "checkouts/flow/target/generated/cljs"]
                        :compiler {:output-to "target/resources/js/flow-sample.js"
                                   :output-dir "target/resources/js/"
                                   :optimizations :whitespace
                                   :pretty-print true}}}}

  :aliases {"dev" ["pdo" "cljsbuild" "auto" "dev," "frodo"]})
