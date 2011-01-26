(ns org.curious.pounce.test.collision
  (:require [org.curious.pounce.math.core :as math]
            [org.curious.pounce.math.matrix :as matrix]
            [org.curious.pounce.body :as body]
            [org.curious.pounce.shape :as shape]
            [org.curious.pounce.collision :as collision]
            [clojure.test :as test]))

(def test-polygon-1 (shape/polygon [0 0] [1 0] [1 1] [0 1]))
(def test-polygon-2 (shape/polygon [0 0] [1/2 0] [1/2 1/2] [0 1/2]))
(def test-circle-1 (shape/circle [0 0] 1))
(def test-circle-2 (shape/circle 1 [0 0] 1))

(def test-body-1 (body/create test-polygon-1))
(def test-body-2 (body/create test-polygon-2))
(def test-body-3 (body/create (matrix/transformation 1 2 (/ math/pi 2)) test-polygon-2))
(def test-body-4 (body/create (shape/polygon 1 [(- (math/sqrt 2)) 0] [(- (/ (math/sqrt 2))) (- (/ (math/sqrt 2)))] [0 0] [(- (/ (math/sqrt 2))) (/ (math/sqrt 2))])
                              (shape/polygon 1 [0 0] [(/ (math/sqrt 2)) (- (/ (math/sqrt 2)))] [(math/sqrt 2) 0] [(/ (math/sqrt 2)) (/ (math/sqrt 2))])))

(test/deftest polygon-polygon-collision-test-1
  (let [actual-collisions (collision/get-shape-collision-with-separating-axis test-polygon-1 (shape/translate test-polygon-1 (matrix/create 1 0)))
        expected-collisions [{:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 0) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 1) (matrix/create 1 0)]}
                             {:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 1) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 1) (matrix/create 1 0)]}]]
    (test/is (collision/collision= (first actual-collisions) (first expected-collisions)))
    (test/is (collision/collision= (second actual-collisions) (second expected-collisions)))))
(test/deftest polygon-polygon-collision-test-2
  (let [actual-collisions (collision/get-shape-collision-with-separating-axis test-polygon-1 (shape/translate test-polygon-2 (matrix/create 1 0)))
        expected-collisions [{:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 0) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 1/2) (matrix/create 1 0)]}
                             {:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 1/2) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 1/2) (matrix/create 1 0)]}]]
    (test/is (collision/collision= (first actual-collisions) (first expected-collisions)))
    (test/is (collision/collision= (second actual-collisions) (second expected-collisions)))))
(test/deftest polygon-polygon-collision-test-3
  (let [actual-collisions (collision/get-shape-collision-with-separating-axis test-polygon-1 (shape/translate test-polygon-2 (matrix/create 1 1/2)))
        expected-collisions [{:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 1/2) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 1) (matrix/create 1 1/2)]}
                             {:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 1) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 1) (matrix/create 1 1/2)]}]]
    (test/is (collision/collision= (first actual-collisions) (first expected-collisions)))
    (test/is (collision/collision= (second actual-collisions) (second expected-collisions)))))
(test/deftest polygon-polygon-collision-test-4
  (let [actual-collisions (collision/get-shape-collision-with-separating-axis test-polygon-1 (shape/translate test-polygon-2 (matrix/create 1 1/4)))
        expected-collisions [{:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 1/4) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 3/4) (matrix/create 1 1/4)]}
                             {:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 3/4) :face1 [(matrix/create 1 0) (matrix/create 1 1)] :face2 [(matrix/create 1 3/4) (matrix/create 1 1/4)]}]]
    (test/is (collision/collision= (first actual-collisions) (first expected-collisions)))
    (test/is (collision/collision= (second actual-collisions) (second expected-collisions)))))
(test/deftest polygon-polygon-collision-test-5
  (let [actual-collisions (collision/get-shape-collision-with-separating-axis test-polygon-1 (shape/translate test-polygon-1 (matrix/create 1 1)))
        expected-collisions [{:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 1)}]]
    (test/is (collision/collision= (first actual-collisions) (first expected-collisions)))))
(test/deftest polygon-polygon-collision-test-6
  (let [actual-collisions (collision/get-shape-collision-with-separating-axis test-polygon-1 (shape/translate test-polygon-1 (matrix/create 1 -1)))
        expected-collisions [{:normal (matrix/create 1 0) :depth 0 :point (matrix/create 1 0)}]]
    (test/is (collision/collision= (first actual-collisions) (first expected-collisions)))))

(test/deftest collisions-test-1
  (let [result-collisions (collision/get-body-collisions-with-separating-axis (assoc test-body-1 :id 1) (assoc test-body-1 :transformation (matrix/transformation 1 0 0) :id 2))
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
    (test/is (collision/collision= (first result-collisions) (first expected-collisions)))
    (test/is (collision/collision= (second result-collisions) (second expected-collisions)))))
(test/deftest collisions-test-2
  (let [result-collisions (collision/get-body-collisions-with-separating-axis (assoc test-body-4 :transformation (matrix/transformation 0 (/ (math/sqrt 2)) 0) :id 1)
                                                (assoc test-body-4 :transformation (matrix/transformation 0 (- (/ (math/sqrt 2))) 0) :id 2))
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
    (test/is (collision/collision= (first result-collisions) (first expected-collisions)))
    (test/is (collision/collision= (second result-collisions) (second expected-collisions)))))


