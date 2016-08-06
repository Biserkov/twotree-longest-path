(ns twotree.direct
  (:require [clojure.data.int-map :as set]
            [twotree.core :refer [combine-on-edge combine-on-face]]))

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