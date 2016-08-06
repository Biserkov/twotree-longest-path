(ns twotree.core-test
  (:require [twotree.core :refer :all]
    ;[twotree.benchmarks :refer :all]
            [clojure.data.int-map :as set]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.properties :as prop']))


(use 'clojure.pprint)


#_(defn max2DistinctFolios [a b k]
  (reduce max (for [i (range 0 k)
                    j (range 0 k)
                    :when (not= i j)]
                (+ (nth a i) (nth b j)))))

#_(defn max3DistinctFolios [a b c k]
  (reduce max (for [i (range 0 k)
                    j (range 0 k)
                    t (range 0 k)
                    :when (distinct? i j t)]
                (+ (nth a i) (nth b j) (nth c t)))))

(comment (defspec max3-finds-max 1000
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

(defspec linear-max2DistinctFolios-equals-naive-implementation 1000
         (prop'/for-all [k gen/s-pos-int :when (>= k 2)
                         v1 (gen/vector gen/pos-int k)
                         v2 (gen/vector gen/pos-int k)]
                        (= (linear-max2DistinctFolios v1 v2 k)
                           (max2DistinctFolios v1 v2 k))))

#_(defspec linear-max3DistinctFolios-equals-naive-implementation 1000
           (prop'/for-all [k gen/s-pos-int :when (>= k 3)
                           v1 (gen/vector gen/pos-int k)
                           v2 (gen/vector gen/pos-int k)
                           v3 (gen/vector gen/pos-int k)]
                          (= (linear-max3DistinctFolios v1 v2 v3 k)
                             (max3DistinctFolios v1 v2 v3 k))))

(defspec max2DistinctFolios-less-than-sum-max 1000
         (prop'/for-all [k gen/s-pos-int :when (>= k 2)
                         v1 (gen/vector gen/pos-int k)
                         v2 (gen/vector gen/pos-int k)]
                        (<= (linear-max2DistinctFolios v1 v2 k)
                            (+ (apply max v1)
                               (apply max v2)))))

(defspec max2DistinctFolios-symmetry 1000
         (prop'/for-all [k gen/s-pos-int :when (>= k 2)
                         v1 (gen/vector gen/pos-int k)
                         v2 (gen/vector gen/pos-int k)]
                        (= (linear-max2DistinctFolios v1 v2 k)
                           (linear-max2DistinctFolios v2 v1 k))))

(defspec max2DistinctFolios-greather-than-each-max 1000
         (prop'/for-all [k gen/s-pos-int :when (>= k 2)
                         v1 (gen/vector gen/pos-int k)
                         v2 (gen/vector gen/pos-int k)]
                        (let [m (linear-max2DistinctFolios v1 v2 k)]
                          (and (>= m (apply max v1))
                               (>= m (apply max v2))))))

(defspec longest-path-is-not-crazy 1000
         (prop'/for-all [k gen/s-pos-int :when (> k 1)]
                        (< 0 (longest-path (Generate2tree k)) k))))