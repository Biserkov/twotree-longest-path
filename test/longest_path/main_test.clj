(ns longest-path.main-test
  (:require [longest-path.generators :refer :all]
            longest-path.iterative
            longest-path.preprocessed
            longest-path.direct
            [clojure.test :refer [deftest is are]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :refer [s-pos-int]]
            [com.gfredericks.test.chuck.properties :refer [for-all]]))

(def num_tests (int 1e4))



(deftest ^:functional trivial
  (let [t (map generate-max-internal-edges-2tree [2 3 4 5 6])
        d (map longest-path.direct/longest-path-length t)
        p (map longest-path.preprocessed/longest-path-length t)
        i (map longest-path.iterative/longest-path-length t)
        results [1 2 3 4 5]]
    (map #(is (= d p i %)) results)))

(deftest ^:functional trivial2
  (let [t (map generate-min-internal-edges-2tree [2 3 4 5 6 100])
        p (map longest-path.preprocessed/longest-path-length t)
        i (map longest-path.iterative/longest-path-length t)
        results [1 2 3 4 4 4]]
    (map #(is (= p i %)) results)))

(defspec ^:functional random-2trees num_tests
         (for-all [n s-pos-int :when (> n 1)]
                  (let [t (generate-random-2tree n)
                        p (longest-path.preprocessed/longest-path-length t)
                        i (longest-path.iterative/longest-path-length t)]
                    (and (= #_d p i)
                         (< 0 i n)))))

(defspec ^:functional min-internal-edges-2trees num_tests
         (for-all [n s-pos-int :when (> n 1)]
                  (let [t (generate-min-internal-edges-2tree n)
                        ;;; d (longest-path.direct/longest-path-length t)
                        ;;; ^ is the slowest, oldest version
                        ;;; will throw java.lang.IllegalArgumentException: Cannot merge int-sets of different density.
                        ;;; due to https://dev.clojure.org/jira/browse/DIMAP-13
                        p (longest-path.preprocessed/longest-path-length t)
                        i (longest-path.iterative/longest-path-length t)]
                    (= #_d p i
                           (if (<= n 4) (dec n) 4)))))

(defspec ^:functional max-internal-edges-2trees num_tests
         (for-all [n s-pos-int :when (> n 1)]
                  (let [t (generate-max-internal-edges-2tree n)
                        ;;; d (longest-path.direct/longest-path-length t)
                        ;;; ^ is the slowest, oldest version
                        p (longest-path.preprocessed/longest-path-length t)
                        i (longest-path.iterative/longest-path-length t)]
                    (= #_d p i (dec n)))))