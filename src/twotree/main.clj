(ns twotree.main
  (:require [twotree.core :refer [read-2tree]]
    ;[twotree.benchmarks :refer :all]
            [twotree.iterative :refer [longest-path-iterative]])
  (:gen-class :main true))

(use 'criterium.core)

(defn -main [file]
  (println (.. (java.time.LocalDateTime/now) toLocalTime toString))
  (bench (->>
           file
           slurp read-2tree
           ;(. Integer parseInt) generate-min-internal-edges-2tree :data
           ;(. Integer parseInt) generate-max-internal-edges-2tree :data
           longest-path-iterative))
  (println (.. (java.time.LocalDateTime/now) toLocalTime toString)))

;(-main "../../../input/g102400.txt")
;(-main "20")

