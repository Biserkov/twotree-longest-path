(ns twotree.main-test
  (:require [twotree.generators :refer :all]
            [twotree.iterative :refer [longest-path-iterative]]
            [twotree.preprocessed :refer [longest-path-linear]]
            [twotree.direct :refer [longest-path-direct]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :refer [s-pos-int]]
            [clojure.test.check.properties :refer [for-all]]))

(def num_tests (int 1e4))

(defn paths [generator n]
  (if (> n 2)
    (let [t (generator n)
          ;d (longest-path-direct t)
          l (longest-path-linear t)
          i (longest-path-iterative (:data t))]
      (and (= #_d l i)
           (< 1 i n)))
    true))

(defspec ^:functional random-2trees num_tests
         (for-all [n s-pos-int]
                  (paths generate-random-2tree n)))

(defspec ^:functional min-internal-edges-2trees num_tests
         (for-all [n s-pos-int]
                  (paths generate-min-internal-edges-2tree n)))

(defspec ^:functional max-internal-edges-2trees num_tests
         (for-all [n s-pos-int]
                  (paths generate-max-internal-edges-2tree n)))