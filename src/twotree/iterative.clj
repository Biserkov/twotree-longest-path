(ns twotree.iterative
  (:require [twotree.core :refer [combine-on-edge combine-on-face compute-degrees]]))

(defmacro symmetric [label]
  `(let [[a1# a2# a3# a4# a5# a6# a7#] ~label]
     [a1# a2# a5# a6# a3# a4# a7#]))

(defn longest-path-iterative [tree]
  (loop [data tree
         [degrees unprocessed] (compute-degrees tree)
         EdgeLabels (transient {})]
    (let [w (first unprocessed)
          rst (pop unprocessed)
          edge (get data w)
          u (first edge)
          v (second edge)
          degU (dec (get degrees u))
          degV (dec (get degrees v))
          addU (= 2 degU)
          addV (= 2 degV)
          [label1 match1] (if-let [normal (get EdgeLabels [u w])]
                            [(combine-on-edge (persistent! normal)) [u w]]
                            (if-let [reversed (get EdgeLabels [w u])]
                              [(symmetric (combine-on-edge (persistent! reversed))) [w u]]
                              [[1 1 0 0 0 0 0] false]))
          [label2 match2] (if-let [normal (get EdgeLabels [w v])]
                            [(combine-on-edge (persistent! normal)) [w v]]
                            (if-let [reversed (get EdgeLabels [v w])]
                              [(symmetric (combine-on-edge (persistent! reversed))) [v w]]
                              [[1 1 0 0 0 0 0] false]))

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
               (assoc! (cond (and match1 match2) (dissoc! EdgeLabels match1 match2)
                             match1 (dissoc! EdgeLabels match1)
                             match2 (dissoc! EdgeLabels match2)
                             :else EdgeLabels) [u v] (conj! (get EdgeLabels [u v] (transient [])) label)))))))
