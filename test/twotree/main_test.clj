(ns twotree.main-test
  (:require [twotree.benchmarks :refer :all]
            [twotree.iterative :refer [longest-path-iterative]]
            [twotree.preprocessed :refer [longest-path-linear]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.properties :as prop']))

(def num_tests (int 1e4))

(defspec random-2trees num_tests
         (prop'/for-all [n gen/s-pos-int]
                        (if (> n 2)
                          (let [t (generate-random-2tree n)
                                b (longest-path-linear t)
                                c (longest-path-iterative (:data t))]
                            (= b c))
                          true)))

(defspec min-internal-edges-2trees num_tests
         (prop'/for-all [n gen/s-pos-int]
                        (if (> n 2)
                          (let [t (generate-min-internal-edges-2tree n)
                                b (longest-path-linear t)
                                c (longest-path-iterative (:data t))]
                            (= b c))
                          true)))

(defspec max-internal-edges-2trees num_tests
         (prop'/for-all [n gen/s-pos-int]
                        (if (> n 2)
                          (let [t (generate-max-internal-edges-2tree n)
                                b (longest-path-linear t)
                                c (longest-path-iterative (:data t))]
                            (and
                              (= b c)
                              (< 0 b n)))
                          true)))