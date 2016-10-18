(ns twotree.iterative
  (:require [twotree.core :refer [combine-on-edge combine-on-face-proper combine-on-face-left compute-degrees]]))

(defmacro reverse-label [label]
  `(let [[a1# a2# a3# a4# a5# a6# a7#] ~label]
     [a1# a2# a5# a6# a3# a4# a7#]))

(defn compute-label-edge [a b EdgeLabels key]
  (if-let [labels (get EdgeLabels key)]
    (let [label (combine-on-edge (persistent! labels))]
      (if (< b a) (reverse-label label) label))
    false))

(defmacro combine-on-face-right [x]
  `(reverse-label (combine-on-face-left (reverse-label ~x))))

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
          key1 (if (< u w) [u w] [w u])
          key2 (if (< w v) [w v] [v w])
          label1 (compute-label-edge u w EdgeLabels key1)
          label2 (compute-label-edge w v EdgeLabels key2)
          newEdgeLabels (cond (and label1 label2) (dissoc! EdgeLabels key1 key2)
                              label1 (dissoc! EdgeLabels key1)
                              label2 (dissoc! EdgeLabels key2)
                              :else EdgeLabels)
          label (cond (and label1 label2) (combine-on-face-proper label1 label2)
                      label1 (combine-on-face-left label1)
                      label2 (combine-on-face-right label2)
                      :else [2 2 2 1 2 1 1])]
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
