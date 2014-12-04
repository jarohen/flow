(defproject jarohen/flow.contacts ""

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  
  :dependencies [[org.clojure/clojure "1.6.0"]

                 [ring/ring-core "1.2.0"]
                 [compojure "1.1.6"]
                 [hiccup "1.0.5"]

                 [jarohen/flow "0.3.0-SNAPSHOT"]
                 
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/clojurescript "0.0-2371"]]

  :plugins [[jarohen/lein-frodo "0.4.1"]
            [jarohen/simple-brepl "0.1.2"]
            [lein-cljsbuild "1.0.3"]
            [lein-pdo "0.1.1"]

            [com.keminglabs/cljx "0.4.0"]
            [lein-shell "0.4.0"]]

  :frodo/config-resource "contacts-config.edn"

  :source-paths ["src" "target/generated/clj"]

  :resource-paths ["resources" "target/resources"]

  :cljx {:builds [{:source-paths ["common-src"]
                   :output-path "target/generated/clj"
                   :rules :clj}

                  {:source-paths ["common-src"]
                   :output-path "target/generated/cljs"
                   :rules :cljs}]}

  :cljsbuild {:builds {:dev
                       {:source-paths ["ui-src" "target/generated/cljs" "../../src" "../../target/generated/cljs"]
                        :compiler {:output-to "target/resources/js/contacts.js"
                                   :output-dir "target/resources/js/"
                                   :optimizations :none
                                   :pretty-print true}}

                       :prod
                       {:source-paths ["ui-src"]
                        :compiler {:output-to "target/resources/js/contacts.js"
                                   :optimizations :advanced

                                   :pretty-print true}}}}

  :aliases {"dev" ["do"
                   ["shell" "mkdir" "-p"
                    "target/generated/clj"
                    "target/generated/cljs"
                    "target/resources"]
                   ["cljx" "once"]
                   ["pdo"
                    ["cljx" "auto"]
                    ["cljsbuild" "auto" "dev"]
                    "frodo"]]
            
            "start" ["do"
                     ["cljx" "once"]
                     ["cljsbuild" "once" "prod"]
                     ["trampoline" "frodo"]]})
