(defproject jarohen/flow "0.2.0-SNAPSHOT"
  :description "Lightweight library to help you write dynamic CLJS webapps"

  :url "https://github.com/james-henderson/flow"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0-alpha4"]]

  :exclusions [org.clojure/clojure]

  :plugins [[com.keminglabs/cljx "0.4.0"]]

  :source-paths ["src" "target/generated/clj"]
  
  :hooks [cljx.hooks]

  :filespecs [{:type :path
               :path "target/generated/cljs"}]

  :cljx {:builds [{:source-paths ["src"]
                   :output-path "target/generated/clj"
                   :rules :clj}

                  {:source-paths ["src"]
                   :output-path "target/generated/cljs"
                   :rules :cljs}]})
