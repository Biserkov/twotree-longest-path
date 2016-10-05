(ns twotree.main
  (:require [twotree.iterative :refer [longest-path-iterative]]
            [twotree.core :refer [read-2tree]]
            [twotree.generators :refer :all])
  (:gen-class :main true))

(use 'criterium.core)

(defn -main [file]
  (println (.. (java.time.LocalDateTime/now) toLocalTime toString))
  (bench (->>
           file
           read-2tree
           ;(. Integer parseInt) generate-min-internal-edges-2tree :data
           ;(. Integer parseInt) generate-max-internal-edges-2tree :data
           longest-path-iterative))
  (println (.. (java.time.LocalDateTime/now) toLocalTime toString)))
