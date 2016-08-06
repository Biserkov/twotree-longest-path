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





  #_(def fail {:root [0 3], :data (read-2tree
                                    "#m[
                                   0 #s #{1 2 3 4},
                                   1 #s #{0 2 3 4 5},
                                   2 #s #{0 1 5 6 7},
                                   3 #s #{0 1},
                                   4 #s #{0 1},
                                   5 #s #{1 2 6 7},
                                   7 #s #{2 5}
                                   6 #s #{2 5},
                                   ]")})

  #_(->> file
         slurp
         read-2tree
         compute-degrees
         (#(hash-map :root [0 1] :data (doall %))))