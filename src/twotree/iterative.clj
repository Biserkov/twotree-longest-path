(ns twotree.iterative
  (:require [twotree.core :refer [combine-on-edge combine-on-face compute-degrees]]))

(defmacro reverse-label [label]
  `(let [[a1# a2# a3# a4# a5# a6# a7#] ~label]
     [a1# a2# a5# a6# a3# a4# a7#]))

(defn compute-label-edge [a b EdgeLabels]
  (let [reversed (< b a)
        key (if reversed [b a] [a b])
        labels (get EdgeLabels key)]
    (if labels
      (let [label (combine-on-edge (persistent! labels))]
        [(if reversed (reverse-label label) label) key])
      [[1 1 0 0 0 0 0] false])))

(defn longest-path-iterative [tree]
  (loop [data tree
         [degrees unprocessed] (compute-degrees tree)
         EdgeLabels (transient {})]
    (let [w (first unprocessed)
          rst (pop unprocessed)
          edge (get data w)
          u (apply min edge)
          v (apply max edge)
          degU (dec (get degrees u))
          degV (dec (get degrees v))
          addU (= 2 degU)
          addV (= 2 degV)
          [label1 match1] (compute-label-edge u w EdgeLabels)
          [label2 match2] (compute-label-edge w v EdgeLabels)
          newEdgeLabels (cond (and match1 match2) (dissoc! EdgeLabels match1 match2)
                              match1 (dissoc! EdgeLabels match1)
                              match2 (dissoc! EdgeLabels match2)
                              :else EdgeLabels)
          label (combine-on-face label1 label2)]
      (if (= 1 degU degV)
        (first label)
        (recur (assoc! (dissoc! data w)
                       u (disj (data u) w)
                       v (disj (data v) w))
               [(assoc! (dissoc! degrees w)
                        u degU
                        v degV)
                (cond (and addU addV) (conj rst u v)
                      addU (conj rst u)
                      addV (conj rst v)
                      :else rst)]
               (assoc! newEdgeLabels [u v] (conj! (get EdgeLabels [u v] (transient [])) label)))))))
