(ns pounce.math.core
  (:use pounce.math.matrix
        pounce.math.polynomial
        pounce.math.lcp))

(def M (matrix [0 0 1 -2 -3 0 0 -1 1 -1 -1 1 0 0 0 2 -1 0 0 0 3 1 0 0 0] 5 5))
(def q (matrix [-1 -1 2 -1 3]))

(println M)
(println q)
(println (solve-lcp M q))
