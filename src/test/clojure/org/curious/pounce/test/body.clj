(ns org.curious.pounce.test.body
  (:require [org.curious.pounce.body :as body]
            [org.curious.pounce.shape :as shape]
            [org.curious.pounce.math.core :as math]
            [org.curious.pounce.math.matrix :as matrix]
            [clojure.test :as test]))

(def test-polygon-1 (shape/polygon [0 0] [1 0] [1 1] [0 1]))
(def test-polygon-2 (shape/polygon 1 [0 0] [1 0] [1 1] [0 1]))
(def test-body-1 (org.curious.pounce.body.Body. (matrix/transformation 0 0 0)
                                                [(shape/polygon [0 0] [1 0] [1 1] [0 1])]
                                                math/positive-infinity
                                                (matrix/create 0 0)
                                                (matrix/create 0 0)
                                                (matrix/create 0 0)
                                                0
                                                0
                                                0
                                                math/positive-infinity
                                                (matrix/create 1/2 1/2)
                                                false
                                                true))
(def test-body-2 (org.curious.pounce.body.Body. (matrix/transformation 0 0 0)
                                                [(shape/polygon 1 [0 0] [1 0] [1 1] [0 1])]
                                                1/6
                                                (matrix/create 0 0)
                                                (matrix/create 0 0)
                                                (matrix/create 0 0)
                                                0
                                                0
                                                0
                                                1
                                                (matrix/create 1/2 1/2)
                                                false
                                                true))
(def test-body-3 (org.curious.pounce.body.Body. (matrix/transformation 1 2 (/ math/pi 2))
                                                [(shape/polygon 1 [0 0] [1 0] [1 1] [0 1])]
                                                1/6
                                                (matrix/create 0 0)
                                                (matrix/create 0 0)
                                                (matrix/create 0 0)
                                                0
                                                0
                                                0
                                                1
                                                (matrix/create 1/2 1/2)
                                                false
                                                true))
(def test-body-4 (org.curious.pounce.body.Body. (matrix/transformation 0 0 0)
                                               [(shape/polygon 1 [(- (math/sqrt 2)) 0] [(- (/ (math/sqrt 2))) (- (/ (math/sqrt 2)))] [0 0] [(- (/ (math/sqrt 2))) (/ (math/sqrt 2))])
                                                (shape/polygon 1 [0 0] [(/ (math/sqrt 2)) (- (/ (math/sqrt 2)))] [(math/sqrt 2) 0] [(/ (math/sqrt 2)) (/ (math/sqrt 2))])]
                                                1/6
                                                (matrix/create 0 0)
                                                (matrix/create 0 0)
                                                (matrix/create 0 0)
                                                0
                                                0
                                                0
                                                1
                                                (matrix/create 1/2 1/2)
                                                false
                                                true))

(test/deftest body-test
  (test/is (= (body/create test-polygon-1) test-body-1))
  (test/is (= (body/create test-polygon-2) test-body-2))
  (test/is (= (body/create (matrix/transformation 1 2 (/ math/pi 2)) test-polygon-2) test-body-3)))

(test/deftest update-test
  (test/is (= (body/update (assoc test-body-2
                             :angular-velocity (/ math/pi 2)
                             :linear-velocity (matrix/create 1 0)
                             :kinematic true)
                           1)
              (assoc test-body-2
                :angular-velocity (/ math/pi 2)
                :linear-velocity (matrix/create 1 0)
                :transformation (matrix/transformation 2 0 (/ math/pi 2))
                :kinematic true)))
  (test/is (= (body/update (assoc test-body-2
                             :angular-momentum (/ math/pi 6)
                             :linear-momentum (matrix/create 1 0))
                           1)
              (assoc test-body-2
                :angular-velocity math/pi
                :angular-momentum (/ math/pi 6)
                :linear-velocity (matrix/create 1 0)
                :linear-momentum (matrix/create 1 0)
                :transformation (matrix/transformation 2 1 math/pi)))))

(test/deftest collisions-test-1
  (let [result-collisions (body/collisions (assoc test-body-1 :id 1) (assoc test-body-1 :transformation (matrix/transformation 1 0 0) :id 2))
        expected-collisions [{:normal (matrix/create 1 0)
                              :depth 0
                              :point (matrix/create 1 0)
                              :face1 [(matrix/create 1 0) (matrix/create 1 1)]
                              :face2 [(matrix/create 1 1) (matrix/create 1 0)]
                              :body1 1
                              :body2 2}
                             {:normal (matrix/create 1 0)
                              :depth 0
                              :point (matrix/create 1 1)
                              :face1 [(matrix/create 1 0) (matrix/create 1 1)]
                              :face2 [(matrix/create 1 1) (matrix/create 1 0)]
                              :body1 1
                              :body2 2}]]
    (test/is (= (count result-collisions) (count expected-collisions)))
    (test/is (shape/collision= (first result-collisions) (first expected-collisions)))
    (test/is (shape/collision= (second result-collisions) (second expected-collisions)))))
(test/deftest collisions-test-2
  (let [result-collisions (body/collisions (assoc test-body-4 :transformation (matrix/transformation 0 (/ (math/sqrt 2)) 0) :id 1) (assoc test-body-4 :transformation (matrix/transformation 0 (- (/ (math/sqrt 2))) 0) :id 2))
        expected-collisions [{:normal (matrix/create (/ (math/sqrt 2)) (- (/ (math/sqrt 2))))
                              :depth 0
                              :point (matrix/create (- (/ (math/sqrt 2))) 0)
                              :body1 1
                              :body2 2}
                             {:normal (matrix/create (/ (math/sqrt 2)) (- (/ (math/sqrt 2))))
                              :depth 0
                              :point (matrix/create (/ (math/sqrt 2)) 0)
                              :body1 1
                              :body2 2}]]
    (test/is (= (count result-collisions) (count expected-collisions)))
    (test/is (shape/collision= (first result-collisions) (first expected-collisions)))
    (test/is (shape/collision= (second result-collisions) (second expected-collisions)))))
