(ns org.curious.pounce.test.math.matrix
  (:require [clojure.test :as test]
            [org.curious.pounce.math.core :as math]
            [org.curious.pounce.math.matrix :as matrix]))

(def test-matrix-2x1 (org.curious.pounce.math.matrix.Matrix. (doto (make-array matrix/primitive 2 1)
                                                               (matrix/aset-primitive 0 0 1)
                                                               (matrix/aset-primitive 1 0 2))))
(def test-matrix-2x2 (org.curious.pounce.math.matrix.Matrix. (doto (make-array matrix/primitive 2 2)
                                                               (matrix/aset-primitive 0 0 1)
                                                               (matrix/aset-primitive 1 0 2)
                                                               (matrix/aset-primitive 0 1 3)
                                                               (matrix/aset-primitive 1 1 4))))
(def test-matrix-2x3 (org.curious.pounce.math.matrix.Matrix. (doto (make-array matrix/primitive 2 3)
                                                               (matrix/aset-primitive 0 0 1)
                                                               (matrix/aset-primitive 1 0 2)
                                                               (matrix/aset-primitive 0 1 3)
                                                               (matrix/aset-primitive 1 1 4)
                                                               (matrix/aset-primitive 0 2 5)
                                                               (matrix/aset-primitive 1 2 6))))
(def test-matrix-3x2 (org.curious.pounce.math.matrix.Matrix. (doto (make-array matrix/primitive 3 2)
                                                               (matrix/aset-primitive 0 0 1)
                                                               (matrix/aset-primitive 1 0 2)
                                                               (matrix/aset-primitive 2 0 3)
                                                               (matrix/aset-primitive 0 1 4)
                                                               (matrix/aset-primitive 1 1 5)
                                                               (matrix/aset-primitive 2 1 6))))
(def test-matrix-3x3 (org.curious.pounce.math.matrix.Matrix. (doto (make-array matrix/primitive 3 3)
                                                               (matrix/aset-primitive 0 0 1)
                                                               (matrix/aset-primitive 1 0 2)
                                                               (matrix/aset-primitive 2 0 3)
                                                               (matrix/aset-primitive 0 1 4)
                                                               (matrix/aset-primitive 1 1 5)
                                                               (matrix/aset-primitive 2 1 6)
                                                               (matrix/aset-primitive 0 2 7)
                                                               (matrix/aset-primitive 1 2 8)
                                                               (matrix/aset-primitive 2 2 9))))
(def test-multi-matrix-2x2 (org.curious.pounce.math.matrix.MultiMatrix. 2 2 (doto (make-array org.curious.pounce.math.matrix.Matrix 2 2)
                                                                              (aset 0 0 test-matrix-2x2)
                                                                              (aset 1 0 nil)
                                                                              (aset 0 1 nil)
                                                                              (aset 1 1 (org.curious.pounce.math.matrix.Matrix. (doto (make-array matrix/primitive 2 2)
                                                                                                                                  (matrix/aset-primitive 0 0 -1)
                                                                                                                                  (matrix/aset-primitive 1 0 -2)
                                                                                                                                  (matrix/aset-primitive 0 1 -3)
                                                                                                                                  (matrix/aset-primitive 1 1 -4)))))))
(def test-multi-matrix-3x3 (org.curious.pounce.math.matrix.MultiMatrix. 2 2 (make-array org.curious.pounce.math.matrix.Matrix 3 3)))
(def test-translation test-matrix-2x1)
(def test-rotation (org.curious.pounce.math.matrix.Matrix. (doto (make-array matrix/primitive 2 2)
                                                             (matrix/aset-primitive 0 0 0)
                                                             (matrix/aset-primitive 1 0 1)
                                                             (matrix/aset-primitive 0 1 -1)
                                                             (matrix/aset-primitive 1 1 0))))
(def test-transformation (org.curious.pounce.math.matrix.Transformation. test-translation test-rotation))

(test/deftest matrix-equals-test-1
  (test/is (= test-matrix-3x3 test-matrix-3x3)))
(test/deftest matrix-equals-test-2
  (test/is (= test-multi-matrix-2x2 test-multi-matrix-2x2)))
(test/deftest matrix-test-1
  (test/is (= (matrix/create test-matrix-2x1) test-matrix-2x1)))
(test/deftest matrix-test-2
  (test/is (= (matrix/create 1 2) test-matrix-2x1)))
(test/deftest matrix-test-3
  (test/is (= (matrix/create [1 2 3] [4 5 6] [7 8 9]) test-matrix-3x3)))
(test/deftest multi-matrix-test
  (test/is (= (matrix/multi-matrix 2 2 3 3) test-multi-matrix-3x3)))
(test/deftest get-test-1
  (test/is (= (matrix/get test-matrix-3x3 1 1) 5)))
(test/deftest get-test-2
  (test/is (= (matrix/get test-multi-matrix-2x2 2 2) -1)))
(test/deftest get-test-3
  (test/is (= (matrix/get test-multi-matrix-2x2 1 2) 0)))
(test/deftest rotation-matrix-test
  (test/is (= (matrix/rotation-matrix (/ math/pi 2)) test-rotation)))
(test/deftest transformation-test-1
  (test/is (= (matrix/transformation 1 2 (/ math/pi 2)) test-transformation)))
(test/deftest transformation-test-2
  (test/is (= (matrix/transformation (matrix/create 1 2) (/ math/pi 2)) test-transformation)))
(test/deftest clone-test-1
  (test/is (= (matrix/clone test-matrix-3x3) test-matrix-3x3)))
(test/deftest clone-test-2
  (test/is (= (matrix/clone test-multi-matrix-2x2) test-multi-matrix-2x2)))
(test/deftest set-test-1
  (test/is (= (matrix/set (matrix/clone test-matrix-3x3) 1 1 1) (matrix/create [1 2 3] [4 1 6] [7 8 9]))))
(test/deftest set-test-2
  (test/is (= (matrix/batch-set (matrix/clone test-multi-matrix-2x2) 1 1 (matrix/create [10 11] [12 13])) (doto (matrix/multi-matrix 2 2 2 2)
                                                                                                      (matrix/batch-set 0 0 test-matrix-2x2)
                                                                                                      (matrix/batch-set 1 1 (matrix/create [10 11] [12 13]))))))
(test/deftest add-test-1
  (test/is (= (matrix/add test-matrix-2x2 test-matrix-2x2) (matrix/create [2 4] [6 8]))))
(test/deftest add-test-2
  (test/is (= (matrix/add test-matrix-2x2 test-matrix-2x2 test-matrix-2x2) (matrix/create [3 6] [9 12]))))
(test/deftest sub-test-1
  (test/is (= (matrix/sub test-matrix-2x2) (matrix/create [-1 -2] [-3 -4]))))
(test/deftest sub-test-2
  (test/is (= (matrix/sub test-matrix-2x2 test-matrix-2x2) (matrix/create [0 0] [0 0]))))
(test/deftest sub-test-3
  (test/is (= (matrix/sub test-matrix-2x2 test-matrix-2x2 test-matrix-2x2) (matrix/create [-1 -2] [-3 -4]))))
(test/deftest mul-test-1
  (test/is (= (matrix/mul test-matrix-2x2 2) (matrix/create [2 4] [6 8]))))
(test/deftest mul-test-2
  (test/is (= (matrix/mul test-matrix-2x3 test-matrix-3x2) (matrix/create [22 28] [49 64]))))
(test/deftest div-test
  (test/is (= (matrix/div test-matrix-2x2 2) (matrix/create [1/2 1] [3/2 2]))))
(test/deftest x-test
  (test/is (math/eps= (matrix/x test-matrix-2x1) 1)))
(test/deftest y-test
  (test/is (math/eps= (matrix/y test-matrix-2x1) 2)))
(test/deftest dot-test
  (test/is (math/eps= (matrix/dot test-matrix-2x1 test-matrix-2x1) 5)))
(test/deftest cross-test
  (test/is (math/eps= (matrix/cross test-matrix-2x1 test-matrix-2x1) 0)))
(test/deftest length-squared-test
  (test/is (math/eps= (matrix/length-squared test-matrix-2x1) 5)))
(test/deftest length-test
  (test/is (math/eps= (matrix/length test-matrix-2x1) (math/sqrt 5))))
(test/deftest unit-test
  (test/is (= (matrix/unit test-matrix-2x1) (matrix/div test-matrix-2x1 (math/sqrt 5)))))
(test/deftest rotate-test
  (test/is (= (matrix/rotate test-matrix-2x1 (/ math/pi 2)) (matrix/create -2 1))))
(test/deftest transform-test
  (test/is (= (matrix/transform test-matrix-2x1 test-transformation) (matrix/create -1 3))))
(test/deftest gauss-seidel-test
  (test/is (= (matrix/gauss-seidel (matrix/create [16 7] [3 -11]) (matrix/create 11 13) 100) (matrix/create 0.8121827411157131 -0.6649746192900007)))
  (test/is (= (matrix/gauss-seidel (matrix/create [16 7] [3 -11]) (matrix/create 11 13) math/eps) (matrix/create 0.8121827411157131 -0.6649746192900007))))
