(ns org.curious.pounce.test.body
  (:require [org.curious.pounce.body :as body]
            [org.curious.pounce.shape :as shape]
            [org.curious.pounce.math.core :as math]
            [org.curious.pounce.math.matrix :as matrix]
            [clojure.test :as test]))

(def test-body-1 (Body. (matrix/transformation 0 0 0)
                        [(shape/polygon [0 0] [1 0] [1 1] [0 1])]
                        math/positive-infinity
                        (matrix/create 0 0)
                        (matrix/create 0 0)
                        0
                        0
                        math/positive-infinity
                        (matrix/create 1/2 1/2)
                        false))
(def test-body-2 (Body. (matrix/transformation 0 0 0)
                        [(shape/polygon [0 0] [1 0] [1 1] [0 1])]
                        1/6
                        (matrix/create 0 0)
                        (matrix/create 0 0)
                        0
                        0
                        1
                        (matrix/create 1/2 1/2)
                        false))
(def test-body-3 (Body. (matrix/transformation 1 2 (/ math/pi 2))
                        [(shape/polygon [0 0] [1 0] [1 1] [0 1])]
                        1/6
                        (matrix/create 0 0)
                        (matrix/create 0 0)
                        0
                        0
                        1
                        (matrix/create 1/2 1/2)
                        false))

(test/deftest body-test
  (test/is (= (body/create (shape/polygon [0 0] [1 0] [1 1] [0 1])) test-body-1))
  (test/is (= (body/create (shape/polygon 1 [0 0] [1 0] [1 1] [0 1])) test-body-2))
  (test/is (= (body/create (matrix/transformation 1 2 (/ math/pi 2)) (shape/polygon 1 [0 0] [1 0] [1 1] [0 1])) test-body-3)))

(test/deftest update-test)
