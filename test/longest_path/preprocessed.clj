(ns longest-path.preprocessed
  (:require [longest-path.iterative :refer [combine-on-edge compute-degrees]]
            [longest-path.direct :refer [combine-on-face]]
            clojure.data.int-map))

(defn hashCode [x y]
  (+ x (bit-shift-left y 25)))

(defn preprocess-tree [tree [x y]]
  (loop [data tree
         [degrees unprocessed] (compute-degrees tree)
         EdgeNodes (transient (clojure.data.int-map/int-map (hashCode x y) (transient (clojure.data.int-map/intersection (data x) (data y)))))]
    (if (empty? unprocessed)
      EdgeNodes
      (let [vertex (first unprocessed)
            rst (pop unprocessed)]
        (if (or (> 2 (get degrees vertex))
                (= vertex x)
                (= vertex y))
          (recur data [degrees rst] EdgeNodes)
          (let [edge (get data vertex)
                u (apply min edge)
                v (apply max edge)
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
                     (assoc! EdgeNodes (hashCode u v) (conj! (get EdgeNodes (hashCode u v) (transient [])) vertex))))))))))

(defn compute-label-linear [[u v w] edge? edge->faces]
  (if edge?
    (if-let [folios (get edge->faces (hashCode (min u v) (max u v)))]
      (combine-on-edge (map (fn [a]
                              (compute-label-linear (conj [u v] a) false edge->faces))
                            (persistent! folios)))
      [1 1 0 0 0 0 0])
    (combine-on-face (compute-label-linear [u w] true edge->faces)
                     (compute-label-linear [w v] true edge->faces))))

(defn longest-path-length [graph]
  (let [[u v] (first graph)
        root [u (first v)]]
    (first (compute-label-linear root true (preprocess-tree graph root)))))
