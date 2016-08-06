(ns twotree.core
  (:require [clojure.data.int-map :as set]))

(alias 'm 'clojure.data.int-map)

(use 'clojure.pprint)
(defrecord twotree [root data])


(defmacro positive [f x]
  `(if (< 0 ~x)
     ~f
     0))

(defn cof [[a1 a2 a3 a4 a5 a6 a7]
                       [b1 b2 b3 b4 b5 b6 b7]]
  ;(println "cof" [a1 a2 a3 a4 a5 a6 a7] [b1 b2 b3 b4 b5 b6 b7])
  (let [l2 (+ a2 b2)
        l3 (max (positive (inc b6) b6)
                (positive (+ a2 b3) b3)
                (inc b2)
                (positive (inc b5) b5)
                (positive (+ 1 b2 a6) a6))
        l4 (max a3 a4 (+ a2 b4))
        l5 (max (positive (inc a4) a4)
                (positive (+ b2 a5) a5)
                (inc a2)
                (positive (inc a3) a3)
                (positive (+ 1 a2 b4) b4))
        l6 (max b5 b6 (+ b2 a6))
        l7 (max (+ a4 b6)
                (+ a4 b5)
                (+ a4 b2)
                (+ a2 b6)
                (+ a3 b6)
                (+ a2 b7)
                (+ a7 b2))
        l1 (max l2 l3 l4 l5 l6 a1 b1 (inc l7) (+ (max a5 a6)
                                                 (max b3 b4)))]
    [l1 l2 l3 l4 l5 l6 l7]))

(def combine-on-face (memoize cof))

(defn max2 [a k]
  (loop [m 0 s 0 idx 0 i 0]
    (if (= i k)
      [m s idx]
      (let [ai (nth a i)]
        (if (> ai m)
          (recur ai m i (inc i))
          (recur m (max ai s) idx (inc i)))))))

(defmacro max2-macro [a k]
  `(loop [m# 0 s# 0 idx# 0 i# 0]
    (if (= i# ~k)
      [m# s# idx#]
      (let [ai# (nth ~a i#)]
        (if (> ai# m#)
          (recur ai# m# i# (inc i#))
          (recur m# (max ai# s#) idx# (inc i#)))))))

(defn max3 [a k]
  (loop [m 0 s 0 t 0 idx 0 idy 0 i 0]
    (if (= i k)
      [m s t idx idy]
      (let [ai (nth a i)]
        (cond
          (>= ai m s) (recur ai m s i idx (inc i))
          (>= m ai s) (recur m ai s idx i (inc i))
          :else (recur m s (max ai t) idx idy (inc i)))))))

(defn linear-max2DistinctFolios-non-macro [a b k]
  (let [[ma sa ia] (max2-macro a k)
        [mb sb ib] (max2-macro b k)]
    (if (not= ia ib)
      (+ ma mb)
      (max (+ ma sb)
           (+ sa mb)))))

(defmacro linear-max2DistinctFolios [a b k]
  `(let [[ma# sa# ia#] (max2-macro ~a ~k)
        [mb# sb# ib#] (max2-macro ~b ~k)]
    (if (not= ia# ib#)
      (+ ma# mb#)
      (max (+ ma# sb#)
           (+ sa# mb#)))))

(defn linear-max3DistinctFolios [a b c k]
  (let [[ma sa ta ia ja] (max3 a k)
        [mb sb tb ib jb] (max3 b k)
        [mc sc tc ic jc] (max3 c k)
        ab-blocker (if (= ia ib) jb ib)
        ac-blocker (if (= ia ic) jc ic)
        ba-blocker (if (= ib ia) ja ia)
        bc-blocker (if (= ib ic) jc ic)
        ca-blocker (if (= ic ia) ja ia)
        cb-blocker (if (= ic ib) jb ib)
        ]

    (max (+ ma                                              ;a b c
            (if (= ia ib) sb mb)
            (condp = ic
              ia (if (= jc ab-blocker) tc sc)
              ab-blocker (if (= jc ia) tc sc)
              ic mc))
         (+ ma                                              ;a c b
            (if (= ia ic) sc mc)
            (condp = ib
              ia (if (= jb ac-blocker) tb sb)
              ac-blocker (if (= jb ia) tb sb)
              ib mb))
         (+ mb                                              ;b c a
            (if (= ic ib) sc mc)
            (condp = ia
              ib (if (= ja bc-blocker) ta sa)
              bc-blocker (if (= ja ib) ta sa)
              ia ma))
         (+ mb                                              ;b a c
            (if (= ia ib) sa ma)
            (condp = ic
              ib (if (= jc ba-blocker) tc sc)
              ba-blocker (if (= jc ib) tc sc)
              ic mc))
         (+ mc                                              ;c a b
            (if (= ic ia) sa ma)
            (condp = ib
              ic (if (= jb ca-blocker) tb sb)
              ca-blocker (if (= jb ic) tb sb)
              ib mb))
         (+ mc                                              ;c b a
            (if (= ic ib) sb mb)
            (condp = ia
              ic (if (= ja cb-blocker) ta sa)
              cb-blocker (if (= ja ic) ta sa)
              ia ma)))))

(defn coe [labels]
  ;(println "coe" (apply str labels))
  (let [k (count labels)]
    (if (= k 1)
      (first labels)
      (let [[a1 a2 a3 a4 a5 a6 a7] (apply map vector labels)
            l7 (max (apply max a7)
                    (linear-max2DistinctFolios a4 a6 k))
            l2 (apply max a2)
            l4 (apply max a4)
            l6 (apply max a6)
            l3 (max (apply max a3)
                    (linear-max2DistinctFolios a2 a6 k))
            l5 (max (apply max a5)
                    (linear-max2DistinctFolios a2 a4 k))
            l1 (max (apply max a1)
                    l2, l3, l4, l5, l6
                    (linear-max2DistinctFolios a3 a4 k)
                    (linear-max2DistinctFolios a5 a6 k)
                    (linear-max2DistinctFolios a2 a7 k)
                    (linear-max2DistinctFolios a4 a4 k)
                    (linear-max2DistinctFolios a6 a6 k)
                    (if (>= k 3)
                      (linear-max3DistinctFolios a2 a4 a6 k)
                      0))]
        ;(println "coe" (apply str labels))
        [l1 l2 l3 l4 l5 l6 l7]))))

(def combine-on-edge (memoize coe))

(defn compute-degrees [tree]
  (loop [result (transient (m/int-map))
         d-seq tree
         deg2 (list)]
    (if (empty? d-seq)
      [result deg2]
      (let [[vertex neighbours] (first d-seq)
            degree (count neighbours)]
        (recur (assoc! result vertex degree)
               (rest d-seq)
               (if (= degree 2) (conj deg2 vertex) deg2))))))








(require 'clojure.edn)
(defn read-2tree [a-str]
  (clojure.edn/read-string {:readers {'s set/int-set
                                      'm #(apply m/int-map %)}} a-str))

#_(defn write-2tree-to-file [t f]
  (clojure.string/replace (str "#m[" (subs s 1 (dec (.length s))) "]") "#{" "#s #{")
  )


#_(def read-time (fn [n]
                 ;(println n)
                 (let [
                       ;t (Generate2tree n)
                       ;s (str (:data t))

                       tree (->> n
                                 ;(str "g" n ".txt")
                                 slurp
                                 read-2tree
                                 (#(hash-map :root [0 1] :data (doall %))))
                       ;crazy (generate-crazy n)
                       ]
                   ;(spit (str "g" n ".txt") t-str)
                   (time (longest-path-linear tree))
                   ;(println n)
                   ;(pprint (class (second (first (preprocess-tree tree)))))
                   ;(println (new java.util.Date))
                   )))

