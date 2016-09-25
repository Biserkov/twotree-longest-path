(defproject twotree "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha12"]
                 [criterium "0.4.4"]
                 [org.clojure/data.int-map "0.2.3"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]
                                  [com.gfredericks/test.chuck "0.2.6"]
                                  ]}
             :uberjar {:aot :all}}
  :main twotree.main)
