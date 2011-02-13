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
  (let [actual-collisions (collision/get-shape-collision-with-separating-axis test-polygon-1 (shape/translate test-polygon-1 (matrix/column 1 0)))
        expected-collisions [{:normal (matrix/column 1 0) :depth 0 :point (matrix/column 1 0) :face1 [(matrix/column 1 0) (matrix/column 1 1)] :face2 [(matrix/column 1 1) (matrix/column 1 0)]}
                             {:normal (matrix/column 1 0) :depth 0 :point (matrix/column 1 1) :face1 [(matrix/column 1 0) (matrix/column 1 1)] :face2 [(matrix/column 1 1) (matrix/column 1 0)]}]]
    (test/is (math/map-eps= (first actual-collisions) (first expected-collisions)))
    (test/is (math/map-eps= (second actual-collisions) (second expected-collisions)))))
(test/deftest polygon-polygon-collision-test-2
  (let [actual-collisions (collision/get-shape-collision-with-separating-axis test-polygon-1 (shape/translate test-polygon-2 (matrix/column 1 0)))
        expected-collisions [{:normal (matrix/column 1 0) :depth 0 :point (matrix/column 1 0) :face1 [(matrix/column 1 0) (matrix/column 1 1)] :face2 [(matrix/column 1 1/2) (matrix/column 1 0)]}
                             {:normal (matrix/column 1 0) :depth 0 :point (matrix/column 1 1/2) :face1 [(matrix/column 1 0) (matrix/column 1 1)] :face2 [(matrix/column 1 1/2) (matrix/column 1 0)]}]]
    (test/is (math/map-eps= (first actual-collisions) (first expected-collisions)))
    (test/is (math/map-eps= (second actual-collisions) (second expected-collisions)))))
(test/deftest polygon-polygon-collision-test-3
  (let [actual-collisions (collision/get-shape-collision-with-separating-axis test-polygon-1 (shape/translate test-polygon-2 (matrix/column 1 1/2)))
        expected-collisions [{:normal (matrix/column 1 0) :depth 0 :point (matrix/column 1 1/2) :face1 [(matrix/column 1 0) (matrix/column 1 1)] :face2 [(matrix/column 1 1) (matrix/column 1 1/2)]}
                             {:normal (matrix/column 1 0) :depth 0 :point (matrix/column 1 1) :face1 [(matrix/column 1 0) (matrix/column 1 1)] :face2 [(matrix/column 1 1) (matrix/column 1 1/2)]}]]
    (test/is (math/map-eps= (first actual-collisions) (first expected-collisions)))
    (test/is (math/map-eps= (second actual-collisions) (second expected-collisions)))))
(test/deftest polygon-polygon-collision-test-4
  (let [actual-collisions (collision/get-shape-collision-with-separating-axis test-polygon-1 (shape/translate test-polygon-2 (matrix/column 1 1/4)))
        expected-collisions [{:normal (matrix/column 1 0) :depth 0 :point (matrix/column 1 1/4) :face1 [(matrix/column 1 0) (matrix/column 1 1)] :face2 [(matrix/column 1 3/4) (matrix/column 1 1/4)]}
                             {:normal (matrix/column 1 0) :depth 0 :point (matrix/column 1 3/4) :face1 [(matrix/column 1 0) (matrix/column 1 1)] :face2 [(matrix/column 1 3/4) (matrix/column 1 1/4)]}]]
    (test/is (math/map-eps= (first actual-collisions) (first expected-collisions)))
    (test/is (math/map-eps= (second actual-collisions) (second expected-collisions)))))
(test/deftest polygon-polygon-collision-test-5
  (let [actual-collisions (collision/get-shape-collision-with-separating-axis test-polygon-1 (shape/translate test-polygon-1 (matrix/column 1 1)))
        expected-collisions [{:normal (matrix/column 1 0) :depth 0 :point (matrix/column 1 1)}]]
    (test/is (math/map-eps= (first actual-collisions) (first expected-collisions)))))
(test/deftest polygon-polygon-collision-test-6
  (let [actual-collisions (collision/get-shape-collision-with-separating-axis test-polygon-1 (shape/translate test-polygon-1 (matrix/column 1 -1)))
        expected-collisions [{:normal (matrix/column 1 0) :depth 0 :point (matrix/column 1 0)}]]
    (test/is (math/map-eps= (first actual-collisions) (first expected-collisions)))))

(test/deftest body-collisions-test-1
  (let [result-collisions (collision/get-body-collisions-with-separating-axis (assoc test-body-1 :id 1) (assoc test-body-1 :transformation (matrix/transformation 1 0 0) :id 2))
        expected-collisions [{:normal (matrix/column 1 0)
                              :depth 0
                              :point (matrix/column 1 0)
                              :face1 [(matrix/column 1 0) (matrix/column 1 1)]
                              :face2 [(matrix/column 1 1) (matrix/column 1 0)]
                              :body1 1
                              :body2 2}
                             {:normal (matrix/column 1 0)
                              :depth 0
                              :point (matrix/column 1 1)
                              :face1 [(matrix/column 1 0) (matrix/column 1 1)]
                              :face2 [(matrix/column 1 1) (matrix/column 1 0)]
                              :body1 1
                              :body2 2}]]
    (test/is (= (count result-collisions) (count expected-collisions)))
    (test/is (math/map-eps= (first result-collisions) (first expected-collisions)))
    (test/is (math/map-eps= (second result-collisions) (second expected-collisions)))))
(test/deftest body-collisions-test-2
  (let [result-collisions (collision/get-body-collisions-with-separating-axis (assoc test-body-4 :transformation (matrix/transformation 0 (/ (math/sqrt 2)) 0) :id 1)
                            (assoc test-body-4 :transformation (matrix/transformation 0 (- (/ (math/sqrt 2))) 0) :id 2))
        expected-collisions [{:normal (matrix/column (/ (math/sqrt 2)) (- (/ (math/sqrt 2))))
                              :depth 0
                              :point (matrix/column (- (/ (math/sqrt 2))) 0)
                              :body1 1
                              :body2 2}
                             {:normal (matrix/column (/ (math/sqrt 2)) (- (/ (math/sqrt 2))))
                              :depth 0
                              :point (matrix/column (/ (math/sqrt 2)) 0)
                              :body1 1
                              :body2 2}]]
    (test/is (= (count result-collisions) (count expected-collisions)))
    (test/is (math/map-eps= (first result-collisions) (first expected-collisions)))
    (test/is (math/map-eps= (second result-collisions) (second expected-collisions)))))

(test/deftest collision-detection-test
  (let [result-collisions (collision/separating-axis-collision-detection [(assoc test-body-4 :transformation (matrix/transformation 0 (/ (math/sqrt 2)) 0) :id 1)
                                                                          (assoc test-body-4 :transformation (matrix/transformation 0 (- (/ (math/sqrt 2))) 0) :id 2)
                                                                          (assoc test-body-1 :transformation (matrix/transformation (+ (/ (math/sqrt 2)) (/ 1 2 (math/sqrt 2)))
                                                                                                                                    (+ (/ (math/sqrt 2)) (/ 1 2 (math/sqrt 2)))
                                                                                                                                    0)
                                                                                 :id 3)])
        expected-collisions [{:normal (matrix/column (/ (math/sqrt 2)) (- (/ (math/sqrt 2))))
                              :depth 0
                              :point (matrix/column (- (/ (math/sqrt 2))) 0)
                              :body1 1
                              :body2 2}
                             {:normal (matrix/column (/ (math/sqrt 2)) (- (/ (math/sqrt 2))))
                              :depth 0
                              :point (matrix/column (/ (math/sqrt 2)) 0)
                              :body1 1
                              :body2 2}
                             {:normal (matrix/column (/ (math/sqrt 2)) (/ (math/sqrt 2)))
                              :depth 0
                              :point (matrix/column (+ (/ (math/sqrt 2)) (/ 1 2 (math/sqrt 2))) (+ (/ (math/sqrt 2)) (/ 1 2 (math/sqrt 2))))
                              :body1 1
                              :body2 3}]]
    (test/is (= (count result-collisions) (count expected-collisions)))
    (test/is (math/map-eps= (first result-collisions) (first expected-collisions)))
    (test/is (math/map-eps= (second result-collisions) (second expected-collisions)))
    (test/is (math/map-eps= (nth result-collisions 2) (nth expected-collisions 2)))))

(test/deftest collision-response-test-1
  (let [bodies {1 (assoc test-body-1 :id 1)
                2 (assoc (body/create (matrix/transformation 1/2 1 (/ math/pi 4)) (shape/polygon 1 [0 0] [1 0] [1 1] [0 1])) :id 2)}
        collisions (collision/separating-axis-collision-detection (vals bodies))
        result-response (collision/velocity-based-collision-response bodies collisions {2 {:force (matrix/column 0 -1)}} (/ 60) ())
        expected-response {1 {:force (matrix/column 0 -1) :torque 0 :velocity matrix/zero} 2 {:force (matrix/column 0 1) :torque 0 :velocity matrix/zero}}]
    (test/is (math/map-eps= result-response expected-response))))

(test/deftest collision-response-test-2
  (let [bodies {1 (assoc test-body-1 :id 1)
                2 (assoc (body/create (matrix/transformation 0 1 0) (shape/polygon 1 [0 0] [1 0] [1 1] [0 1])) :id 2)}
        collisions (collision/separating-axis-collision-detection (vals bodies))
        result-response (collision/velocity-based-collision-response bodies collisions {2 {:force (matrix/column 0 -1)}} (/ 60) ())
        expected-response {1 {:force (matrix/column 0 -1) :torque 0 :velocity matrix/zero} 2 {:force (matrix/column 0 1) :torque 0 :velocity matrix/zero}}]
    (test/is (math/map-eps= result-response expected-response))))

(test/deftest collision-response-test-3
  (let [bodies {1 (assoc (body/create (matrix/transformation 0 0 0) (shape/polygon 1 [0 0] [1 0] [1 1] [0 1])) :id 1)
                2 (assoc (body/create (matrix/transformation 0 1 0) (shape/polygon 1 [0 0] [1 0] [1 1] [0 1])) :id 2)}
        collisions (collision/separating-axis-collision-detection (vals bodies))
        result-response (collision/velocity-based-collision-response bodies collisions {2 {:force (matrix/column 0 -1)}} (/ 60) ())
        expected-response {1 {:force (matrix/column 0 -1/2) :torque 0 :velocity matrix/zero} 2 {:force (matrix/column 0 1/2) :torque 0 :velocity matrix/zero}}]
    (test/is (math/map-eps= result-response expected-response))))

