(ns twotree.main
  (:require [twotree.iterative :refer [longest-path-iterative]]
            [twotree.core :refer [read-2tree]]
            [twotree.generators :refer :all])
  (:gen-class :main true))

(use 'criterium.core)

(defn -main [tree-type input]
  (println (.. (java.time.LocalDateTime/now) toLocalTime toString))
  (let [f longest-path-iterative]
    (bench
      (condp = tree-type
        "fs"  (->> input read-2tree f)
        "rnd" (->> input (. Integer parseInt) generate-random-2tree :data f)
        "min" (->> input (. Integer parseInt) generate-min-internal-edges-2tree :data f)
        "max" (->> input (. Integer parseInt) generate-max-internal-edges-2tree :data f))))
  (println (.. (java.time.LocalDateTime/now) toLocalTime toString)))