(ns twotree.core
  (:require [clojure.data.int-map :as set]))

(use 'clojure.pprint)
(defrecord twotree [root data])

(def test-graph {:root [1 2]
                 :data {1 (set/int-set #{2 3 4})
                        2 (set/int-set #{1 3 6 7})
                        3 (set/int-set #{1 2 4 5 6 7})
                        4 (set/int-set #{1 3 5})
                        5 (set/int-set #{3 4})
                        6 (set/int-set #{2 3})
                        7 (set/int-set #{2 3})
                        }})


(def test-graph2 {:root [1 2]
                  :data {1 (set/int-set #{2 3 4 5})
                         2 (set/int-set #{1 3 4 6})
                         3 (set/int-set #{1 2 5})
                         4 (set/int-set #{1 2 6 7})
                         5 (set/int-set #{1 3})
                         6 (set/int-set #{2 4 7})
                         7 (set/int-set #{4 6})
                         }})

(def test-graph3 {:root [1 2]
                  :data {1 (set/int-set #{2 3 4})
                         2 (set/int-set #{1 3 6 7})
                         3 (set/int-set #{1 2 4 5 6 7})
                         4 (set/int-set #{1 3 5 8})
                         5 (set/int-set #{3 4 8})
                         6 (set/int-set #{2 3})
                         7 (set/int-set #{2 3})
                         8 (set/int-set #{4 5})

                         }})

(defn edges [graph]
  (doall (mapcat (fn [[k neib]]
                   (map (fn [i] (vector k i))
                        (remove (fn [i] (< i k)) neib)))
                 (:data graph))))


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

(defn simple? [{[u v] :root
                G     :data}]
  (second (set/intersection (G u) (G v))))

(defn compute-label [{:keys [data root] :as graph} & complex]
  ;(println (count data) complex)
  (cond (= (data (first root)) (set/int-set (rest root))) [1 1 0 0 0 0 0]
        complex (->> (split-edge graph)
                     (map compute-label)
                     combine-on-edge)
        :simple (let [[H1 H2] (split-face graph)]
                  (combine-on-face (compute-label H1 (simple? H1))
                                   (compute-label H2 (simple? H2))))))

(defn longest-path [graph]
  (first (compute-label graph :complex)))

(defn -main []
  (println (longest-path test-graph)))


(defn Generate2tree [n]
  {:pre [(> n 1)]}
  (loop [acc (transient {0 (set/int-set [1]), 1 (set/int-set [0])})
         i 2]
    (if (= i n)
      {:root [0 1], :data (persistent! acc)}
      (let [k (int (rand i))
            Nk (acc k)
            l (rand-nth (seq Nk))]
        (recur (assoc! acc
                       i (set/int-set [k l])
                       k (conj Nk i)
                       l (conj (acc l) i))
               (inc i))))))

(use 'criterium.core)
;(println (longest-path test-graph))
;(println (longest-path test-graph2))

(require 'clojure.edn)
(defn read-2tree [a-str]
  (clojure.edn/read-string {:readers {'s set/int-set
                                      'm #(apply set/int-map %)}} a-str))

(defn compute-degrees [tree]
  (into (set/int-map)
        (map (fn [[a b]] (vector a (count b))) (:data tree))))

(defn deg2 [degrees]
  (->> degrees
       (filter (fn [[_ b]] (= b 2)))
       (map first)
       (into                                                ;[]
         clojure.lang.PersistentQueue/EMPTY)))

(defn preprocess-tree [tree]
  (let [[x y] (:root tree)]
    (loop [data (transient (:data tree))
           degrees (transient (compute-degrees tree))
           unprocessed (deg2 degrees)
           EdgeNodes (transient {[x y] (set/intersection (data x) (data y))})]
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
                  addV (and (not= v x) (not= v y) (= 2 degV))
                  new-unprocessed (cond (and addU addV) (conj (conj rst v) u)
                                        addU (conj rst u)
                                        addV (conj rst v)
                                        :else rst)]
              ;(println vertex (vec unprocessed))
              (recur (assoc! data
                           u (disj (data u) vertex)
                           v (disj (data v) vertex))
                   (assoc! degrees
                           u degU
                           v degV)
                   new-unprocessed
                   (if (= degU 1)
                     EdgeNodes
                     (assoc! EdgeNodes [u v] (conj (get EdgeNodes [u v]) vertex)))))))))))

(defn compute-label-linear [node type edge->faces]
  ;(println node type)
  (cond
        (= type :edge) (let [folios (or (get edge->faces node)
                                        (get edge->faces (reverse node)))]
                         ;(println "faces" node folios)
                         (if folios
                           (combine-on-edge (map (fn [a]
                                                   (compute-label-linear (conj node a) :face edge->faces))
                                                 folios))
                           [1 1 0 0 0 0 0]))
        (= type :face) (let [[u v w] node]
                         (combine-on-face (compute-label-linear [u w] :edge edge->faces)
                                          (compute-label-linear [w v] :edge edge->faces)))))

(defn longest-path-linear [graph]
  (first (compute-label-linear (:root graph) :edge (preprocess-tree graph))))

;(pprint (preprocess-tree test-graph))
;(println (longest-path-linear test-graph))

;(pprint (preprocess-tree test-graph3))

(deftype FaceNode [childA childB])
(deftype EdgeNode [id children])

#_(defn prep [data]
  (let [edges ]))

#_(println (prep {0 #{1 2 3}
                1 #{0 2 3}
                2 #{0 1}
                3 #{0 1}}))


;(println (FaceNode. 1 2))
;(println (->FaceNode 1 2))

(def read-time (fn [n]
                 ;(println n)
                 (let [
                       ;t (Generate2tree n)
                       ;s (str (:data t))
                       ; t-str (clojure.string/replace (str "#m[" (subs s 1 (dec (.length s))) "]") "#{" "#s #{")
                       tree (->> (str "g" n ".txt")
                              slurp
                              read-2tree
                              (#(hash-map :root [0 1] :data (doall %))))
                       ; degrees (transient (compute-degrees tree))
                       ]
                   ;(spit (str "g" n ".txt") t-str)
                   ;(println (time (longest-path tree)))
                   (println (longest-path tree))
                   (println (longest-path-linear tree))
                   (pprint (preprocess-tree tree))
                   ;(println (new java.util.Date))
                   ;(bench (preprocess-tree tree))
                   )))
(read-time 7)
(comment
  (read-time 100)
(read-time 200)
(read-time 400)
(read-time 800)
(read-time 1600)
(read-time 3200)
(read-time 6400)
(read-time 12800)
(read-time 25600)
(read-time 51200)
(read-time 102400)
(read-time 204800)
(read-time 409600)
(read-time 819200)
(read-time 1638400)
(read-time 3276800)
)
;(doall (take 16 (map read-time (iterate #(* 2 %) 100))))
;(doall (take 11 (map read-time (iterate #(+ 2 %) 10))))

;(get (first (preprocess-tree test-graph)) #{2 3})
;(print (class (get (update {} 5 conj 1) 5)))
