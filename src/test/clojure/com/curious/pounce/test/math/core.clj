(ns com.curious.pounce.test.math.core
  (:use com.curious.pounce.math.core
        clojure.test))

(deftest math-infinite-test
  (is (is-infinite positive-infinity))
  (is (is-infinite negative-infinity))
  (is (not (is-infinite 0))))
(deftest math-sin (is (eps= (sin (/ pi 2)) 1)))
(deftest math-cos (is (eps= (cos (/ pi 2)) 0)))
(deftest math-pow (is (eps= (pow 2 3) 8)))
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
