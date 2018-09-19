(defproject twotree.longest-path "0.1.1"
  :description "Library for computing longest paths in 2-trees in O(n) time"
  :url "https://github.com/Biserkov/twotree-longest-path"
  :scm {:name "git" :url "https://github.com/Biserkov/twotree-longest-path"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.int-map "0.2.4"]]
  :deploy-repositories [["releases"  {:sign-releases false :url "https://clojars.org/repo"}]
                        ["snapshots" {:sign-releases false :url "https://clojars.org/repo"}]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]
                                  [com.gfredericks/test.chuck "0.2.6"]]}
             :uberjar {:aot :all}}
  :test-selectors {:default (fn [m] (:functional m))
                   :all (fn [m] true)}
  :aliases {"test-all" ["test" ":all"]})
