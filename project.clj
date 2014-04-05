(defproject wator "0.1.0-SNAPSHOT"
  :description "Clojurescript Wator simulation, as described in The Armchair Universe, by A. K. Dwedney."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cljsbuild "1.0.2"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2173"]
                 [org.clojure/tools.nrepl "0.2.3"]]
  :source-paths ["src/clj"]
  :cljsbuild {:builds [{:source-paths ["src/cljs"]
                        :compiler {:output-to "resources/public/wator.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}]})
