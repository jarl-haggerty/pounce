(ns com.curious.pounce.test.shape
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:use com.curious.pounce.shape
        com.curious.pounce.math.math
        com.curious.pounce.math.matrix
        com.curious.pounce.render
        clojure.test))

(def test-polygon-1 (Polygon. [(matrix/create 0 0) (matrix/create 1 0) (matrix/create 1 1) (matrix/create 0 1)]
                              math/positive-infinity
                              (matrix/create 1/2 1/2)
                              [(matrix/create 0 -1) (matrix/create 1 0) (matrix/create 0 1) (matrix/create -1 0)]
                              math/positive-infinity))
(def test-polygon-2 (Polygon. [(matrix/create 0 0) (matrix/create 1 0) (matrix/create 1 1) (matrix/create 0 1)]
                              1
                              (matrix/create 1/2 1/2)
                              [(matrix/create 0 -1) (matrix/create 1 0) (matrix/create 0 1) (matrix/create -1 0)]
                              1/6))
(def test-circle-1 (Circle. (matrix/create 0 0)
                            math/positive-infinity
                            1
                            math/positive-infinity))
(def test-circle-2 (Circle. (matrix.create 0 0)
                            1
                            1
                            1/2))

(test/deftest polygon-test
  (test/is (= (shape/polygon [0 0] [1 0] [1 1] [0 1]) test-polygon-1))
  (test/is (= (shape/polygon 1 [0 0] [1 0] [1 1] [0 1]) test-polygon-2)))
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
              {:start 0 :start-points [(matrix/create 0 0) (matrix/create 0 1)]
               :stop 1 :stop-points [(matrix/create 1 0) (matrix/create 1 1)]}))
  (test/is (= (shape/projection test-polygon-1 (matrix/create (/ (math/sqrt 2)) (/ math/sqrt 2)))
              {:start 0 :start-points [(matrix/create 0 0)]
               :stop (math/sqrt 2) :stop-points [(matrix/create 1 1)]})))
(test/deftest circle-projection-test
  (test/is (= (shape/projection test-circle-1 (matrix/create 1 0))
              {:start -1 :start-points [(matrix/create -1 0)]
               :stop 1 :stop-points [(matrix/create 1 0)]})))
(test/deftest polygon-transform-test
  (test/is (= (shape/transform test-polygon-1 (matrix/transformation 1 2 (/ math/pi 2))) (shape/polygon [0 2] [1 2] [1 3] [0 3]))))
(test/deftest polygon-translation-test
  (test/is (= (shape/translate test-polygon-1 (matrix/create 1 2)) (shape/polygon [1 2] [2 2] [2 3] [1 3]))))
(test/deftest polygon-rotation-test
  (test/is (= (shape/rotate test-polygon-1 (matrix/rotation-matrix (/ math/pi 2))) (shape/polygon [-1 0] [0 0] [0 1] [-1 1]))))
(test/deftest circle-transform-test
  (test/is (= (shape/transform test-circle-1 (matrix/transformation 1 2 (/ math/pi 2))) (shape/circle [1 2] 1))))
(test/deftest circle-translation-test
  (test/is (= (shape/translate test-circle-1 (matrix/create 1 2)) (shape/circle [1 2] 1))))
(test/deftest circle-rotation-test
  (test/is (= (shape/rotate test-circle-1 (matrix/rotation-matrix (/ math/pi 2))) (shape/circle [0 0] 1))))
