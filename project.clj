(defproject twotree "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha13"]
                 [criterium "0.4.4"]
                 [org.clojure/data.int-map "0.2.4"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]
                                  [com.gfredericks/test.chuck "0.2.6"]
                                  ]}
             :uberjar {:aot :all}}
  :test-selectors {:default (fn [m] (:functional m))
                   :all (fn [m] true)}
  :aliases {"test-all" ["test" ":all"]}
  :main twotree.main)
