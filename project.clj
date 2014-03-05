(defproject maze "0.1.0-SNAPSHOT"
  :description "Maze generator and solver"
  :url "http://costa-rica.github.io/maze"
  :license {:name "MIT"
            :url "mit"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2156"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]]
  :plugins [[lein-cljsbuild "1.0.2"]
            [com.cemerick/clojurescript.test "0.2.2"]
            [com.cemerick/austin "0.1.4"]]
  :hooks [leiningen.cljsbuild]
  :profiles {:dev {:plugins [[com.cemerick/clojurescript.test "0.2.3-SNAPSHOT"]]}}
  :cljsbuild {:builds [{:source-paths ["src-cljs" "test"]
                        :compiler {:output-to "target/main.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}
                       {:id "test"
                        :source-paths ["src-cljs" "test"]
                        :notify-command ["phantomjs" :cljs.test/runner "target/main.js"]
                        :compiler {:output-to "target/main.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}]
              :test-commands {"unit-tests" ["phantomjs" :runner "target/main.js"]}})
