(defproject maze "0.1.0-SNAPSHOT"
  :description "Maze generator and solver"
  :url "http://costa-rica.github.io/maze"
  :license {:name "MIT"
            :url "mit"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2156"]]
  :plugins [[lein-cljsbuild "1.0.2"]
            [com.cemerick/clojurescript.test "0.2.2"]
            [com.cemerick/austin "0.1.4"]]
  :profiles {:dev {:plugins [[com.cemerick/clojurescript.test "0.2.3-SNAPSHOT"]]}}
  :cljsbuild {
              :builds [{:source-paths ["src-cljs" "test"]
                        :id "test"
                        :notify-command ["phantomjs" :cljs.test/runner "target/main.js"]
                        :compiler {
                                   :output-to "target/main.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}]})
