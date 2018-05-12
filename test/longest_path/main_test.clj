(ns longest-path.main-test
  (:require [longest-path.generators :refer :all]
            longest-path.iterative
            longest-path.preprocessed
            longest-path.direct
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :refer [s-pos-int]]
            [clojure.test.check.properties :refer [for-all]]))

(def num_tests (int 1e2))

(defn paths [generator n]
  (if (> n 2)
    (let [t (generator n)
          ;d (longest-path.direct/longest-path-length t)
          l (longest-path.preprocessed/longest-path-length t)
          i (longest-path.iterative/longest-path-length (:data t))]
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