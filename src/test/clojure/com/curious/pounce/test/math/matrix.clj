(ns com.curious.pounce.test.math.matrix
  (:require [clojure.test :as test]
            [com.curious.pounce.math.core :as math]
            [com.curious.pounce.math.matrix :as matrix]))

(def test-matrix-2x1 (com.curious.pounce.math.matrix.Matrix. (doto (make-array Float/TYPE 2 1)
                                      (aset-float 0 0 1)
                                      (aset-float 1 0 2))))
(def test-matrix-2x2 (com.curious.pounce.math.matrix.Matrix. (doto (make-array Float/TYPE 2 2)
                            (aset-float 0 0 1)
                            (aset-float 1 0 2)
                            (aset-float 0 1 3)
                            (aset-float 1 1 4))))
(def test-matrix-2x3 (com.curious.pounce.math.matrix.Matrix. (doto (make-array Float/TYPE 2 3)
                            (aset-float 0 0 1)
                            (aset-float 1 0 2)
                            (aset-float 0 1 3)
                            (aset-float 1 1 4)
                            (aset-float 0 2 5)
                            (aset-float 1 2 6))))
(def test-matrix-3x2 (com.curious.pounce.math.matrix.Matrix. (doto (make-array Float/TYPE 3 2)
                            (aset-float 0 0 1)
                            (aset-float 1 0 2)
                            (aset-float 2 0 3)
                            (aset-float 0 1 4)
                            (aset-float 1 1 5)
                            (aset-float 2 1 6))))
(def test-matrix-3x3 (com.curious.pounce.math.matrix.Matrix. (doto (make-array Float/TYPE 3 3)
                            (aset-float 0 0 1)
                            (aset-float 1 0 2)
                            (aset-float 2 0 3)
                            (aset-float 0 1 4)
                            (aset-float 1 1 5)
                            (aset-float 2 1 6)
                            (aset-float 0 2 7)
                            (aset-float 1 2 8)
                            (aset-float 2 2 9))))
(def test-transformation (Transformation. test-translation test-rotation))

(test/deftest matrix-equals-test
  (test/is (= test-matrix-3x3 test-matrix-3x3)))
(test/deftest matrix-test-1
  (test/is (= (matrix/create test-matrix-2x1) test-matrix-2x1)))
(test/deftest matrix-test-2
  (test/is (= (matrix/create 1 2) test-matrix-2x1)))
(test/deftest matrix-test-3
  (test/is (= (matrix/create [1 2 3] [4 5 6] [7 8 9]) test-matrix-3x3)))
(test/deftest rotation-matrix-test
  (test/is (= (matrix/rotation-matrix (/ math/pi 2)) test-rotation)))
(test/deftest transformation-test-1
  (test/is (= (matrix/transformation 1 2 (/ math/pi 2)) test-transform)))
(test/deftest transformation-test-2
  (test/is (= (matrix/transformation (matrix/create 1 2) (/ math/pi 2)) test-transform)))
(test/deftest copy-test
  (test/is (= (matrix/copy test-matrix-3x3) test-matrix-3x3)))
(test/deftest set-test-1
  (test/is (= (matrix/set (matrix/clone test-matrix-3x3) 1 1 test-matrix-2x2) (matrix/create [1 2 3] [4 1 2] [7 3 4]))))
(test/deftest set-test-2
  (test/is (= (matrix/set (matrix/clone test-matrix-3x3) 1 1 1) (matrix/create [1 2 3] [4 1 6] [7 8 9]))))
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
  (test/is (= (matrix/mul test-matrix-2x3 text-matrix-3x2) (matrix/create [21 28] [49 64]))))
(test/deftest div-test
  (test/is (= (matrix/div test-matrix-2x2 2) (matrix/create [1/2 1] [3/2 2]))))
(test/deftest x-test
  (test/is (= (matrix/x test-matrix-2x1) 1)))
(test/deftest y-test
  (test/is (= (matrix/y test-matrix-2x1) 2)))
(test/deftest dot-test
  (test/is (= (matrix/dot test-matrix-2x1 test-matrix-2x1) 5)))
(test/deftest cross-test
  (test/is (= (matrix/cross test-matrix-2x1 test-matrix-2x1) 0)))
(test/deftest length-squared-test
  (test/is (= (matrix/length-squared test-matrix-2x1) 5)))
(test/deftest length-test
  (test/is (= (matrix/length-squared test-matrix-2x1) (math/sqrt 5))))
(test/deftest unit-test
  (test/is (= (matrix/unit test-matrix-2x1) (matrix/div test-matrix-2x1 (math/sqrt 5)))))
(test/deftest normal-test
  (test/is (= (matrix/normal test-matrix-2x1) (matrix/div (matrix/create 2 -1) (math/sqrt 5)))))
(test/deftest transform-test
  (test/is (= (matrix/transform test-matrix-2x1 test-transformation) (matrix/create -1 3))))
