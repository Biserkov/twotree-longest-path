(ns longest-path.main
  (:require longest-path.iterative
            longest-path.core
            [longest-path.generators :refer :all])
  (:gen-class :main true))

(use 'criterium.core)

(defn -main [tree-type input]
  (println (.. (java.time.LocalDateTime/now) toLocalTime toString))
  (let [f longest-path.iterative/longest-path-length]
    (bench
      (condp = tree-type
        "fs"  (->> input longest-path.core/read-2tree f)
        "rnd" (->> input (. Integer parseInt) generate-random-2tree f)
        "min" (->> input (. Integer parseInt) generate-min-internal-edges-2tree f)
        "max" (->> input (. Integer parseInt) generate-max-internal-edges-2tree f))))
  (println (.. (java.time.LocalDateTime/now) toLocalTime toString)))