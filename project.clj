(defproject daqtyl "0.1.0-SNAPSHOT"
  :description "A parametrized, split-hand, concave, columnar, ergonomic keyboard with trackballs"
  :url "https://github.com/uqs/daqtyl.git"
  :main daqtyl
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-auto "0.1.3"]
            [lein-exec "0.3.7"]]
  :aliases {"generate" ["run" "everything"]}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [unicode-math "0.2.0"]
                 [scad-clj "0.5.4-SNAPSHOT"]])


