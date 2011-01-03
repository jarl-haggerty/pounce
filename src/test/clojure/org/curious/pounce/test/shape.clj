(ns org.curious.pounce.test.shape
  (:require [org.curious.pounce.shape :as shape]
            [org.curious.pounce.math.core :as math]
            [org.curious.pounce.math.matrix :as matrix]
            [org.curious.pounce.render :as render]
            [clojure.test :as test]))

(def test-polygon-1 (com.curious.pounce.shape.Polygon. [(matrix/create 0 0) (matrix/create 1 0) (matrix/create 1 1) (matrix/create 0 1)]
                              math/positive-infinity
                              (matrix/create 1/2 1/2)
                              [(matrix/create 0 -1) (matrix/create 1 0) (matrix/create 0 1) (matrix/create -1 0)]
                              math/positive-infinity))
(def test-polygon-2 (com.curious.pounce.shape.Polygon. [(matrix/create 0 0) (matrix/create 1/2 0) (matrix/create 1/2 1/2) (matrix/create 0 1/2)]
                              math/positive-infinity
                              (matrix/create 1/4 1/4)
                              [(matrix/create 0 -1) (matrix/create 1 0) (matrix/create 0 1) (matrix/create -1 0)]
                              math/positive-infinity))
(def test-polygon-3 (com.curious.pounce.shape.Polygon. [(matrix/create 0 0) (matrix/create 1 0) (matrix/create 1 1) (matrix/create 0 1)]
                              1
                              (matrix/create 1/2 1/2)
                              [(matrix/create 0 -1) (matrix/create 1 0) (matrix/create 0 1) (matrix/create -1 0)]
                              1/6))
(def test-circle-1 (com.curious.pounce.shape.Circle. (matrix/create 0 0)
                            math/positive-infinity
                            1
                            math/positive-infinity))
(def test-circle-2 (com.curious.pounce.shape.Circle. (matrix/create 0 0)
                            1
                            1
                            1/2))

(test/deftest polygon-test
  (test/is (= (shape/polygon [0 0] [1 0] [1 1] [0 1]) test-polygon-1))
  (test/is (= (shape/polygon 1 [0 0] [1 0] [1 1] [0 1]) test-polygon-3)))
(test/deftest circle-test
  (test/is (= (shape/circle [0 0] 1) test-circle-1))
  (test/is (= (shape/circle 1 [0 0] 1) test-circle-2)))
(test/deftest polygon-normal-test
  (test/is (= (shape/normals test-polygon-1 (matrix/create 1 0))
              [{:normal (matrix/create 0 -1) :side [(matrix/create 0 0) (matrix/create 1 0)]}
               {:normal (matrix/create 1 0) :side [(matrix/create 1 0) (matrix/create 1 1)]}
               {:normal (matrix/create 0 1) :side [(matrix/create 1 1) (matrix/create 0 1)]}])))
(test/deftest circle-normal-test
  (test/is (= (shape/normals test-circle-1 (matrix/create 1 0))
              [{:normal (matrix/create 1 0) :side [(matrix/create 1 0) (matrix/create 1 math/eps)]}])))
(test/deftest polygon-projection-test
  (test/is (= (shape/projection test-polygon-1 (matrix/create 1 0))
              {:start 0 :start-points [(matrix/create 0 1) (matrix/create 0 0)]
               :stop 1 :stop-points [(matrix/create 1 0) (matrix/create 1 1)]}))
  (test/is (= (shape/projection test-polygon-1 (matrix/create (/ (math/sqrt 2)) (/ (math/sqrt 2))))
              {:start 0 :start-points [(matrix/create 0 0)]
               :stop (math/sqrt 2) :stop-points [(matrix/create 1 1)]})))
(test/deftest circle-projection-test
  (test/is (= (shape/projection test-circle-1 (matrix/create 1 0))
              {:start -1 :start-points [(matrix/create -1 0)]
               :stop 1 :stop-points [(matrix/create 1 0)]})))
(test/deftest polygon-transform-test
  (test/is (= (shape/transform test-polygon-1 (matrix/transformation 1 2 (/ math/pi 2))) (shape/polygon [1 2] [1 3] [0 3] [0 2]))))
(test/deftest polygon-translation-test
  (test/is (= (shape/translate test-polygon-1 (matrix/create 1 2)) (shape/polygon [1 2] [2 2] [2 3] [1 3]))))
(test/deftest polygon-rotation-test
  (test/is (= (shape/rotate test-polygon-1 (matrix/rotation-matrix (/ math/pi 2))) (shape/polygon [0 0] [0 1] [-1 1] [-1 0]))))
(test/deftest circle-transform-test
  (test/is (= (shape/transform test-circle-1 (matrix/transformation 1 2 (/ math/pi 2))) (shape/circle [1 2] 1))))
(test/deftest circle-translation-test
  (test/is (= (shape/translate test-circle-1 (matrix/create 1 2)) (shape/circle [1 2] 1))))
(test/deftest circle-rotation-test
  (test/is (= (shape/rotate test-circle-1 (matrix/rotation-matrix (/ math/pi 2))) (shape/circle [0 0] 1))))
(test/deftest polygon-polygon-collision-test-1
  (let [actual-collisions (shape/collision test-polygon-1 (shape/translate test-polygon-1 (matrix/create 1 0)))
        expected-collisions [{:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 0) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 1) (matrix/create 1 0)]}
                             {:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 1) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 1) (matrix/create 1 0)]}]]
    (test/is (shape/collision= (first actual-collisions) (first expected-collisions)))
    (test/is (shape/collision= (second actual-collisions) (second expected-collisions)))))
(test/deftest polygon-polygon-collision-test-2
  (let [actual-collisions (shape/collision test-polygon-1 (shape/translate test-polygon-2 (matrix/create 1 0)))
        expected-collisions [{:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 0) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 1/2) (matrix/create 1 0)]}
                             {:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 1/2) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 1/2) (matrix/create 1 0)]}]]
    (test/is (shape/collision= (first actual-collisions) (first expected-collisions)))
    (test/is (shape/collision= (second actual-collisions) (second expected-collisions)))))
(test/deftest polygon-polygon-collision-test-3
  (let [actual-collisions (shape/collision test-polygon-1 (shape/translate test-polygon-2 (matrix/create 1 1/2)))
        expected-collisions [{:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 1/2) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 1) (matrix/create 1 1/2)]}
                             {:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 1) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 1) (matrix/create 1 1/2)]}]]
    (test/is (shape/collision= (first actual-collisions) (first expected-collisions)))
    (test/is (shape/collision= (second actual-collisions) (second expected-collisions)))))
(test/deftest polygon-polygon-collision-test-4
  (let [actual-collisions (shape/collision test-polygon-1 (shape/translate test-polygon-2 (matrix/create 1 1/4)))
        expected-collisions [{:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 1/4) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 3/4) (matrix/create 1 1/4)]}
                             {:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 3/4) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 3/4) (matrix/create 1 1/4)]}]]
    (test/is (shape/collision= (first actual-collisions) (first expected-collisions)))
    (test/is (shape/collision= (second actual-collisions) (second expected-collisions)))))
(test/deftest polygon-polygon-collision-test-5
  (let [actual-collisions (shape/collision test-polygon-1 (shape/translate test-polygon-1 (matrix/create 1 1)))
        expected-collisions [{:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 1)}]]
    (test/is (shape/collision= (first actual-collisions) (first expected-collisions)))))
(test/deftest polygon-polygon-collision-test-6
  (let [actual-collisions (shape/collision test-polygon-1 (shape/translate test-polygon-1 (matrix/create 1 -1)))
        expected-collisions [{:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 0)}]]
    (test/is (shape/collision= (first actual-collisions) (first expected-collisions)))))
