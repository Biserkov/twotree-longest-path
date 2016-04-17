(ns twotree.core-test
  (:require [twotree.core :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.properties :as prop']))


(use 'clojure.pprint)
(defspec proba 100
         (prop'/for-all [n gen/s-pos-int :when (>= n 3)]
                        (let [t (Generate2tree n)]
                          (pprint t)
                          (let [
                                a (longest-path t)
                                b (longest-path-linear t)]
                            (= a b)))))