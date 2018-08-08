(ns longest-path.core-test
  (:require [longest-path.iterative :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.properties :as prop']))

(use 'clojure.pprint)

(defspec max3-finds-max 1000
         (prop'/for-all [k gen/s-pos-int :when (>= k 3)
                         v (gen/vector gen/pos-int k)]
                        (= (first (max3 v k))
                           (apply max v))))

(defspec max3-maxes-are-correctly-ordered 1000
         (prop'/for-all [k gen/s-pos-int :when (>= k 3)
                         v (gen/vector gen/pos-int k)]
                        (let [[m s t i j] (max3 v k)]
                          (>= m s t))))

(defspec max3-max-index-is-correct 1000
         (prop'/for-all [k gen/s-pos-int :when (>= k 3)
                         v (gen/vector gen/pos-int k)]
                        (let [[m s t i j] (max3 v k)]
                          (= (nth v i) m))))

(defspec max3-max2-index-is-correct 1000
         (prop'/for-all [k gen/s-pos-int :when (>= k 3)
                         v (gen/vector gen/pos-int k)]
                        (let [[m s t i j] (max3 v k)]
                          (= (nth v j) s))))

(defspec max3-max2-indicies-are-distinct 1000
         (prop'/for-all [k gen/s-pos-int :when (>= k 3)
                         v (gen/vector gen/pos-int k)]
                        (let [[m s t i j] (max3 v k)]
                          (not= i j))))

(defn naive-max2DistinctFolios [a b n]
  (reduce max
          (for [i (range 0 n)
                j (range 0 n)
                :when (not= i j)]
            (+ (nth a i) (nth b j)))))

(defspec max2DistinctFolios-equals-naive-implementation 1000
         (prop'/for-all [k gen/s-pos-int :when (>= k 2)
                         v1 (gen/vector gen/pos-int k)
                         v2 (gen/vector gen/pos-int k)]
                        (= (max2DistinctFolios v1 v2 k)
                           (naive-max2DistinctFolios v1 v2 k))))

(defspec max2DistinctFolios-less-than-sum-max 1000
         (prop'/for-all [k gen/s-pos-int :when (>= k 2)
                         v1 (gen/vector gen/pos-int k)
                         v2 (gen/vector gen/pos-int k)]
                        (<= (max2DistinctFolios v1 v2 k)
                            (+ (apply max v1)
                               (apply max v2)))))

(defspec max2DistinctFolios-symmetry 1000
         (prop'/for-all [k gen/s-pos-int :when (>= k 2)
                         v1 (gen/vector gen/pos-int k)
                         v2 (gen/vector gen/pos-int k)]
                        (= (max2DistinctFolios v1 v2 k)
                           (max2DistinctFolios v2 v1 k))))

(defspec max2DistinctFolios-greather-than-each-max 1000
         (prop'/for-all [k gen/s-pos-int :when (>= k 2)
                         v1 (gen/vector gen/pos-int k)
                         v2 (gen/vector gen/pos-int k)]
                        (let [m (max2DistinctFolios v1 v2 k)]
                          (and (>= m (apply max v1))
                               (>= m (apply max v2))))))

(defn naive-max3DistinctFolios [a b c n]
  (reduce
    max (for [i (range 0 n)
              j (range 0 n)
              k (range 0 n)
              :when (distinct? i j k)]
          (+ (nth a i) (nth b j) (nth c k)))))

(defspec max3DistinctFolios-equals-naive-implementation 100
         (prop'/for-all [k gen/s-pos-int :when (>= k 3)
                         v1 (gen/vector gen/pos-int k)
                         v2 (gen/vector gen/pos-int k)
                         v3 (gen/vector gen/pos-int k)]
                        (= (max3DistinctFolios v1 v2 v3 k)
                           (naive-max3DistinctFolios v1 v2 v3 k))))