(ns twotree.iterative
  (:require [twotree.core :refer [combine-on-edge combine-on-face compute-degrees]]))

(defn symmetric [[a1 a2 a3 a4 a5 a6 a7]]
  [a1 a2 a5 a6 a3 a4 a7])

(defn longest-path-iterative [tree]
  (loop [data (transient tree)
         [degrees unprocessed] (compute-degrees tree)
         EdgeNodes (transient {})]
    ;    (println (keys EdgeNodes))
    (let [vertex (first unprocessed)
          rst (pop unprocessed)
          edge (get data vertex)
          u (first edge)
          v (second edge)
          degU (dec (get degrees u))
          degV (dec (get degrees v))
          addU (= 2 degU)
          addV (= 2 degV)
          cof4o (combine-on-face
                  (if-let [normal (get EdgeNodes [u vertex])]
                    (combine-on-edge (persistent! normal))
                    (if-let [reversed (get EdgeNodes [vertex u])]
                      (symmetric (combine-on-edge (persistent! reversed)))
                      [1 1 0 0 0 0 0]))
                  (if-let [normal (get EdgeNodes [vertex v])]
                    (combine-on-edge (persistent! normal))
                    (if-let [reversed (get EdgeNodes [v vertex])]
                      (symmetric (combine-on-edge (persistent! reversed)))
                      [1 1 0 0 0 0 0])))]
      ;(println u v vertex)
      (if (= 1 degU degV)
        (first cof4o)
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
               (assoc! (dissoc! EdgeNodes [u vertex] [vertex u] [v vertex] [vertex v]) [u v] (conj! (get EdgeNodes [u v] (transient [])) cof4o)))))))
