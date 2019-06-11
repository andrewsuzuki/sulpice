(defproject sulpice "0.1.0-SNAPSHOT"
  :description "Parameterized ergonomic keyboard"
  :url "https://github.com/andrewsuzuki/sulpice"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [scad-clj "0.5.3"]]
  :main ^:skip-aot sulpice.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
