(ns twotree.preprocessed
  (:require [twotree.core :refer [combine-on-edge combine-on-face compute-degrees]]
            [clojure.data.int-map :as set]
            [clojure.data.int-map :as m]))

(defn hashCode [x y]
  ; (println (+ x (bit-shift-left y 25)))
  (+ x (bit-shift-left y 25))
  )

(defn hashDecode [n]
  (let [y (bit-shift-right n 25)
        x (- n (bit-shift-left y 25))]
    [x y]))

(defn preprocess-tree [tree]
  (let [[x y] (:root tree)]
    (loop [data (transient (:data tree))
           [degrees unprocessed] (compute-degrees (:data tree))
           EdgeNodes (transient (m/int-map (hashCode x y) (transient (set/intersection (data x) (data y)))))]
      (if (empty? unprocessed)
        EdgeNodes
        (let [vertex (first unprocessed)
              rst (pop unprocessed)]
          (if (or (> 2 (get degrees vertex))
                  (= vertex x)
                  (= vertex y))
            (recur data [degrees rst] EdgeNodes)
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
                     [(assoc! degrees
                              u degU
                              v degV)
                      (cond (and addU addV) (conj rst u v)
                            addU (conj rst u)
                            addV (conj rst v)
                            :else rst)]
                     (if (= degU 1)
                       EdgeNodes
                       (assoc! EdgeNodes (hashCode u v) (conj! (get EdgeNodes (hashCode u v) (transient [])) vertex)))))))))))

;(print "wtf ")
;(println *clojure-version*)


(defn compute-label-linear [node edge? edge->faces]
  ;(when edge? (println node "\t" (hashCode node) "\t" (hashCode (reverse node))))
  (if edge?
    (let [[y x] node
          folios (or (get edge->faces (hashCode y x))
                     (get edge->faces (hashCode x y)))]
      ;(println folios)
      (if folios
        (combine-on-edge (map (fn [a]
                                (compute-label-linear (conj node a) false edge->faces))
                              (persistent! folios)))
        [1 1 0 0 0 0 0]))
    (let [[u v w] node]
      ;(println [u w] [w v])
      (combine-on-face (compute-label-linear [u w] true edge->faces)
                       (compute-label-linear [w v] true edge->faces)))))

(defn longest-path-linear [graph]
  (first (compute-label-linear (:root graph) true (preprocess-tree graph))))
