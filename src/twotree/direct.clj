(ns twotree.direct
  (:require [clojure.data.int-map :as set]
            [twotree.core :refer [combine-on-edge combine-on-face]]))

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

(defn split-root-edge [graph G1-Gk]
  ;(println "edge" (:root graph))
  (let [[u v] (:root graph)
        G (:data graph)
        G-e (assoc! G u (set/int-set)
                      v (set/int-set))]
    (map #(let [keysNewG (subgraph G-e %1 (G %1))]
           {:root [u v]
            :data (assoc! G u (set/intersection keysNewG (G u))
                            v (set/intersection keysNewG (G v)))})
         G1-Gk)))

(defn split-root-face [{[u v] :root
                        G     :data}
                       w]
  ;(println "face" [u v w])
  (let [Gu (G u)
        Gv (G v)
        Gw (G w)
        G-e (assoc! G u (disj Gu v)
                      v (disj Gv u)
                      w (set/int-set))
        keysNewG (subgraph (dissoc! G-e v) u (G-e u))]
    (vector {:root [u w] :data (assoc! (dissoc! G-e v) w (set/intersection Gw keysNewG))}
            {:root [w v] :data (assoc! (dissoc! G-e u) w (set/difference Gw keysNewG))})))

(defn compute-label-direct [{:keys [data root] :as G}]
  (let [i (set/intersection (data (first root))
                            (data (second root)))]
    (cond (empty? i) [1 1 0 0 0 0 0]
          (second i) (->> (split-root-edge G i)
                          (map compute-label-direct)
                          combine-on-edge)
          :simple (let [[H1 H2] (split-root-face G (first i))]
                    (combine-on-face (compute-label-direct H1)
                                     (compute-label-direct H2))))))

(defn longest-path-direct [graph]
  (first (compute-label-direct graph)))