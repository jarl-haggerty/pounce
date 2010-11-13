(ns pounce.test.math.lcp
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:use pounce.math.lcp
        pounce.math.polynomial
        pounce.math.matrix
        pounce.math.math
        clojure.test))

(deftest lcp-simple
  true)
(deftest lcp-degenerate
  (let [M (matrix [0 0 1 -2 -3 0 0 -1 1 -1 -1 1 0 0 0 2 -1 0 0 0 3 1 0 0 0] 5 5)
        q (matrix [-1 -1 2 -1 3])]
    (is (= (solve-lcp M q)
           {'w '(0 0 0 3/4 0) 'z '(1/4 9/4 1/2 0 1/2)}))))
