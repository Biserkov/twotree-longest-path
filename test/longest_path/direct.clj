(ns longest-path.direct
  (:require [clojure.data.int-map :as set]
            [longest-path.iterative :refer [combine-on-edge]]))

(defmacro positive [f x]
  `(if (< 0 ~x)
     ~f
     0))

(defn cof-impl [[a1 a2 a3 a4 a5 a6 a7]
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

(def combine-on-face
  (let [mem (atom {})]
    (fn [a b]
      (if-let [e (find @mem [a b])]
        (val e)
        (let [ret (cof-impl a b)]
          (swap! mem assoc [a b] ret)
          ret)))))

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

(defn split-root-edge [G u v G1-Gk]
  ;(println "edge" [u v])
  (let [G-e (assoc! G u (set/int-set)
                      v (set/int-set))]
    (map #(let [keysNewG (subgraph G-e %1 (G %1))]
            (assoc! G u (set/intersection keysNewG (G u))
                      v (set/intersection keysNewG (G v))))
         G1-Gk)))

(defn split-root-face [G u v w]
  ;(println "face" [u v w])
  (let [Gu (G u)
        Gv (G v)
        Gw (G w)
        G-e (assoc! G u (disj Gu v)
                      v (disj Gv u)
                      w (set/int-set))
        keysNewG (subgraph (dissoc! G-e v) u (G-e u))]
    [(assoc! (dissoc! G-e v) w (set/intersection Gw keysNewG))
     (assoc! (dissoc! G-e u) w (set/difference   Gw keysNewG))]))

(defn compute-label-direct [G u v]
  (let [i (set/intersection (G u)
                            (G v))]
    (cond (empty? i) [1 1 0 0 0 0 0]
          (second i) (->> (split-root-edge G u v i)
                          (map #(compute-label-direct % u v))
                          combine-on-edge)
          :simple (let [w (first i)
                        [H1 H2] (split-root-face G u v w)]
                    (combine-on-face (compute-label-direct H1 u w)
                                     (compute-label-direct H2 w v))))))

(defn longest-path-length [graph]
  (let [[v neib] (first graph)]
    (first (compute-label-direct graph v (first neib)))))