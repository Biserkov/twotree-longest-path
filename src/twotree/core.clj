(ns twotree.core
  (:require [clojure.data.int-map :as set]))

(use 'clojure.pprint)
(defrecord twotree [root data])


(defmacro positive [f x]
  `(if (< 0 ~x)
     ~f
     0))

(defn combine-on-face [[a1 a2 a3 a4 a5 a6 a7]
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

(defn max2 [a k]
  (loop [m 0 s 0 idx 0 i 0]
    (if (= i k)
      [m s idx]
      (let [ai (nth a i)]
        (if (> ai m)
          (recur ai m i (inc i))
          (recur m (max ai s) idx (inc i)))))))

(defn max3 [a k]
  (loop [m 0 s 0 t 0 idx 0 idy 0 i 0]
    (if (= i k)
      [m s t idx idy]
      (let [ai (nth a i)]
        (cond
          (>= ai m s) (recur ai m s i idx (inc i))
          (>= m ai s) (recur m ai s idx i (inc i))
          :else (recur m s (max ai t) idx idy (inc i)))))))

(defn linear-max2DistinctFolios [a b k]
  (let [[ma sa ia] (max2 a k)
        [mb sb ib] (max2 b k)]
    (if (not= ia ib)
      (+ ma mb)
      (max (+ ma sb)
           (+ sa mb)))))

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

(defn combine-on-edge [labels]
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
        [l1 l2 l3 l4 l5 l6 l7]))))

(defn subgraph2 [a s r u Gu v Gv]
  (loop [r1 r
         keysNewG (set/int-set [s])]
    (if (seq r1)
      (let [f (first r1)
            Nf (get a f)]
        (recur (set/union (disj r1 f)
                          (set/difference Nf
                                          keysNewG))
               (conj keysNewG f)))
      {:root [u v]
       :data (assoc a u (set/intersection keysNewG Gu)
                      v (set/intersection keysNewG Gv))})))

(defn split-edge [graph]
  ;(println "edge" (:root graph))
  (let [[u v] (:root graph)
        G (:data graph)
        components (set/intersection (G u) (G v))
        G-e (assoc G u (set/int-set)
                     v (set/int-set))]
    (map #(subgraph2 G-e %1 (G %1) u (G u) v (G v)) components)))

(defn subgraph [a s r]
  (loop [r1 r
         keysNewG (set/int-set [s])]
    (if (seq r1)
      (let [f (first r1)
            Nf (get a f)]
        (recur (set/union (disj r1 f)
                          (set/difference Nf
                                          keysNewG))
               (conj keysNewG f)))
      keysNewG)))

(defn split-face [{[u v] :root
                   G     :data}]
  (let [Gu (G u)
        Gv (G v)
        w (first (set/intersection Gu Gv))
        Gw (G w)
        G-e (assoc G u (disj Gu v)
                     v (disj Gv u)
                     w (set/int-set))
        keysNewG (subgraph (dissoc G-e v) u (G-e u))]
    ;(println "face" [u v w])
    (vector {:root [u w] :data (assoc (dissoc G-e v) w (set/intersection Gw keysNewG))}
            {:root [w v] :data (assoc (dissoc G-e u) w (set/difference Gw keysNewG))})))

(defn simple? [{[u v] :root
                G     :data}]
  (second (set/intersection (G u) (G v))))

(defn compute-label-direct [{:keys [data root] :as graph} & complex]
  ;(println (count data) complex)
  (cond (= (data (first root)) (set/int-set (rest root))) [1 1 0 0 0 0 0]
        complex (->> (split-edge graph)
                     (map compute-label-direct)
                     combine-on-edge)
        :simple (let [[H1 H2] (split-face graph)]
                  (combine-on-face (compute-label-direct H1 (simple? H1))
                                   (compute-label-direct H2 (simple? H2))))))

(defn longest-path-direct [graph]
  (first (compute-label-direct graph :complex)))







(defn compute-degrees [tree]
  (loop [result (transient (set/int-map))
         d-seq tree
         deg2 (list)
         ]
    (if (empty? d-seq)
      [result deg2]
      (let [[a b] (first d-seq)
            c (count b)]
        (recur (assoc! result a c)
               (rest d-seq)
               (if (= c 2) (conj deg2 a) deg2))))))

(defn preprocess-tree [tree]
  (let [[x y] (:root tree)
        [wtf1 wtf2] (compute-degrees (:data tree))]
    (loop [data (transient (:data tree))
           degrees wtf1
           unprocessed wtf2
           EdgeNodes (transient {
                                 [x y] (transient (set/intersection (data x) (data y)))
                                 })]
      (if (empty? unprocessed)
        (persistent! EdgeNodes)
        (let [vertex (first unprocessed)
              rst (pop unprocessed)]
          (if (or (> 2 (get degrees vertex))
                  (= vertex x)
                  (= vertex y))
            (recur data degrees rst EdgeNodes)
            (let [edge (get data vertex)
                  u (first edge)
                  v (second edge)
                  degU (dec (get degrees u))
                  degV (dec (get degrees v))
                  addU (and (not= u x) (not= u y) (= 2 degU))
                  addV (and (not= v x) (not= v y) (= 2 degV))]
              (recur (assoc! data
                           u (disj (data u) vertex)
                           v (disj (data v) vertex))
                   (assoc! degrees
                           u degU
                           v degV)
                   (cond (and addU addV) (conj rst u v )
                         addU (conj rst u)
                         addV (conj rst v)
                         :else rst)
                   (if (= degU 1)
                     EdgeNodes
                     (assoc! EdgeNodes [u v] (conj! (get EdgeNodes [u v] (transient [])) vertex)))))))))))

(defn compute-label-linear [node edge edge->faces]
  (if edge
    (let [folios (or (get edge->faces node)
                     (get edge->faces (reverse node)))]
                         (if folios
                           (combine-on-edge (map (fn [a]
                                                   (compute-label-linear (conj node a) false edge->faces))
                                                 (persistent! folios)))
                           [1 1 0 0 0 0 0]))
        (let [[u v w] node]
                         (combine-on-face (compute-label-linear [u w] true edge->faces)
                                          (compute-label-linear [w v] true edge->faces)))))

(defn longest-path-linear [graph]
  (first (compute-label-linear (:root graph) true (preprocess-tree graph))))

(require 'clojure.edn)
(defn read-2tree [a-str]
  (clojure.edn/read-string {:readers {'s set/int-set
                                      'm #(apply set/int-map %)}} a-str))

(defn -main []
  ;(println (longest-path test-graph))
  )