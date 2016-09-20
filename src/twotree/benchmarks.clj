(ns twotree.benchmarks
  (:require [clojure.data.int-map :as set]))

(defn generate-random-2tree [n]
  {:pre [(> n 1)]}
  (loop [acc (transient (set/int-map 0 (set/int-set [1]), 1 (set/int-set [0])))
         i 2]
    (if (= i n)
      {:root [0 1], :data acc}
      (let [k (int (rand i))
            Nk (acc k)
            l (rand-nth (seq Nk))]
        (recur (assoc! acc
                       i (set/int-set [k l])
                       k (conj Nk i)
                       l (conj (acc l) i))
               (inc i))))))

(defn generate-max-internal-edges-2tree [n]
  {:pre [(> n 1)]}
  (loop [acc (transient (set/int-map 0 (set/int-set [1]),
                                     1 (set/int-set [0])))
         i 2]
    (if (= i n)
      {:root [0 1], :data acc}
      (let [k (dec i)
            Nk (acc k)
            l (dec k)]
        (recur (assoc! acc
                       i (set/int-set [k l])
                       k (conj Nk i)
                       l (conj (acc l) i))
               (inc i))))))



(defn generate-min-internal-edges-2tree [n]
  {:pre [(> n 1)]}
  (loop [i 2
         acc (transient (set/int-map 0 (set/dense-int-set (range 1 n))
                                     1 (set/dense-int-set (conj (range 2 n) 0))))]
    (if (= i n)
      {:root [0 1]
       :data acc}
      (recur (inc i)
             (assoc! acc i (set/int-set [0 1]))))))