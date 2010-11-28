(ns com.curious.pounce.test.math.math
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:use com.curious.pounce.math.math
        clojure.test))

(deftest math-infinite-test
  (is (is-infinite positive-infinity))
  (is (is-infinite negative-infinity))
  (is (not (is-infinite 0))))
(deftest math-sin (is (eps= (sin (/ pi 2)) 1)))
(deftest math-cos (is (eps= (cos (/ pi 2)) 0)))
(deftest math-pow (is (= (pow 2 3) 8)))
(deftest math-sqrt (is (eps= (sqrt 64) 8)))
(deftest math-abs
  (is (= (abs 5) 5))
  (is (= (abs -5) 5)))
(deftest math-ceil
  (is (= (ceil 10) 10))
  (is (= (ceil 5/2) 3))
  (is (= (ceil 5/4) 2)))
(deftest math-floor
  (is (= (floor 10) 10))
  (is (= (floor 5/2) 2))
  (is (= (floor 5/4) 1)))
(deftest math-round
  (is (= (round 10) 10))
  (is (= (round 5/2) 3))
  (is (= (round 5/4) 1)))
(deftest math-circular-vector (is (= ((circular-vector [1 2 3]) 8) 3)))
(deftest math-eps
  (is (eps= 0 0))
  (is (eps= 0 1e-20))
  (is (not (eps= 0 1e-5))))
(deftest math-plus
  (is (= (+ 1 1) 2))
  (is (not= (+ 1 2) 2)))
(deftest math-minus
  (is (= (- 10) -10))
  (is (not= (- 10) 11)))
(deftest math-multiply
  (is (= (* 2 2) 4))
  (is (not= (* 2 3) 4)))
(deftest math-divide
  (is (= (/ 10) 1/10))
  (is (not= (/ 10) 1/11)))
(deftest math-less
  (is (< 5 10))
  (is (not (< 15 10))))
(deftest math-less-eq
  (is (<= 5 10))
  (is (<= 10 10))
  (is (not (<= 15 10))))
(deftest math-greater
  (is (> 15 10))
  (is (not (> 5 10))))
(deftest math-greater-eq
  (is (>= 15 10))
  (is (>= 10 10))
  (is (not (>= 5 10))))
(deftest math-key
  (is (= (min-key identity 5 2 1 3 4) 1))
  (is (= (max-key identity 1 4 5 2 3) 5)))
(deftest math=
  (is (= 1.0 1.0))
  (is (= 1 1.0))
  (is (= 1.0 1))
  (is (= 1 1))
  (is (= 1.00000000001 1.0))
  (is (= 1.0 1.00000000001))
  (is (= 1.00000000001 1))
  (is (= 1 1.00000000001))
  (is (not= 1 2))
  (is (not= 1.0 2))
  (is (not= 1 2.0))
  (is (not= 1.0 2.0))
  (is (= '(1 2 3) '(1 2 3)))
  (is (not= '(1 2 3) '(2 2 3)))
  (is (= {:1 1 :2 2} {:1 1 :2 2}))
  (is (not= {:1 1 :3 2} {:1 1 :2 2}))
  (is (= [1 2 3] [1 2 3]))
  (is (not= [1 2 4] [1 2 3])))
(deftest math-seq=
  (is (seq= [1 2 3] [1 2 3]))
  (is (seq= '(1 2 3) '(1 2 3)))
  (is (not-seq= [1 2] [1 2 3]))
  (is (not-seq= [1 2 4] [1 2 3]))
  (is (not-seq= '(1 2) '(1 2 3)))
  (is (not-seq= '(1 2 4) '(1 2 3)))
  (is (seq= [1 2 3] [1 2 3] [1 2 3]))
  (is (seq= '(1 2 3) [1 2 3] '(1 2 3)))
  (is (not-seq= [1 2 3] [1 2 3] [1 2]))
  (is (not-seq= [1 2 3] [1 2] [1 2 3]))
  (is (not-seq= [1 2 3] [1 2 3] [1 2 4]))
  (is (not-seq= [1 2 3] [1 2 4] [1 2 3])))
