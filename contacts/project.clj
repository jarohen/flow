(defproject jarohen/flow.contacts ""

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  
  :dependencies [[org.clojure/clojure "1.6.0"]

                 [ring/ring-core "1.2.0"]
                 [compojure "1.1.5"]
                 [hiccup "1.0.4"]

                 [prismatic/dommy "0.1.2"]

                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [org.clojure/tools.reader "0.8.3"]

                 [jarohen/flow "0.1.0-SNAPSHOT"]

                 [gaka "0.3.0"]]

  :plugins [[jarohen/lein-frodo "0.3.0-rc2"]
            [lein-cljsbuild "1.0.3"]
            [lein-pdo "0.1.1"]
            [com.keminglabs/cljx "0.3.2"]]

  :frodo/config-resource "contacts-config.edn"

  :source-paths ["src/clojure" "target/generated/clj" "../src"]

  :resource-paths ["resources" "target/resources"]

  :cljx {:builds [{:source-paths ["src/cljx" "../src"]
                   :output-path "target/generated/clj"
                   :rules :clj}

                  {:source-paths ["src/cljx" "../src"]
                   :output-path "target/generated/cljs"
                   :rules :cljs}]}

  :cljsbuild {:builds {:dev
                       {:source-paths ["src/cljs" "target/generated/cljs" "../src" "../target/generated/cljs"]
                        :compiler {:output-to "target/resources/js/contacts.js"
                                   :output-dir "target/resources/js/"
                                   :optimizations :whitespace
                                   :pretty-print true}}}}

  :aliases {"dev" ["pdo"
                   ["cljx" "auto"]
                   ["cljsbuild" "auto" "dev"]
                   ["frodo"]]})
