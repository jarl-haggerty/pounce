(ns pounce.test.body
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:use pounce.body
        pounce.math.math
        pounce.math.matrix
        pounce.shape
        clojure.test
        clojure.set))
(comment
(deftest body-equal-1
  (is (= (with-meta {:transform identity-transform
                     :shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                     :moment-of-inertia positive-infinity
                     :linear-momentum (matrix 0 0)
                     :angular-momentum 0
                     :mass positive-infinity
                     :center-of-mass (matrix 1/2 1/2)
                     :center (matrix 1/2 1/2)
                     :radius (/ (sqrt 2))}
           {:type :body})
         (with-meta {:transform identity-transform
                     :shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                     :moment-of-inertia positive-infinity
                     :linear-momentum (matrix 0 0)
                     :angular-momentum 0
                     :mass positive-infinity
                     :center-of-mass (matrix 1/2 1/2)
                     :center (matrix 1/2 1/2)
                     :radius (/ (sqrt 2))}
           {:type :body}))))

(deftest body-equal-2
  (is (= (with-meta {:transform identity-transform
                     :shapes [(polygon [0 0] [1 0] [1 1] [0 1]) (circle 10 [2 3] 5)]
                     :moment-of-inertia positive-infinity
                     :linear-momentum (matrix 0 0)
                     :angular-momentum 0
                     :mass positive-infinity
                     :center-of-mass (matrix 1/2 1/2)
                     :center (matrix 1/2 1/2)
                     :radius (/ (sqrt 2))}
           {:type :body})
         (with-meta {:transform identity-transform
                     :shapes [(polygon [0 0] [1 0] [1 1] [0 1]) (circle 10 [2 3] 5)]
                     :moment-of-inertia positive-infinity
                     :linear-momentum (matrix 0 0)
                     :angular-momentum 0
                     :mass positive-infinity
                     :center-of-mass (matrix 1/2 1/2)
                     :center (matrix 1/2 1/2)
                     :radius (/ (sqrt 2))}
           {:type :body}))))

(deftest radius-polygon-1
  (is (= (radius (matrix 0 0) [(polygon [0 0] [1 0] [1 1] [0 1])]) (sqrt 2))))
(deftest radius-polygon-2
  (is (= (radius (matrix 1/2 1/2) [(polygon [0 0] [1 0] [1 1] [0 1])]) (/ (sqrt 2)))))
(deftest radius-polygon-3
  (is (= (radius (matrix 3/4 3/4) [(polygon [0 0] [1 0] [1 1] [0 1])]) (/ (* 3 (sqrt 2)) 4))))
(deftest radius-polygon-4
  (is (= (radius (matrix 2 0) [(polygon [0 0] [1 0] [1 1] [0 1])]) (sqrt 5))))
(deftest radius-polygon-5
  (is (= (radius (matrix 3/4 3/4) [(polygon [0 0] [1 0] [1 1] [0 1]) (polygon [1 1] [2 1] [2 2] [1 2])]) (/ (* 5 (sqrt 2)) 4))))
(deftest radius-circle-1
  (is (= (radius (matrix 0 0) [(circle [0 0] 2)]) 2)))
(deftest radius-circle-2
  (is (= (radius (matrix 1/2 1/2) [(circle [0 0] 2)]) (+ 2 (/ (sqrt 2))))))
(deftest radius-circle-3
  (is (= (radius (matrix 3/4 3/4) [(circle [0 0] 2)]) (+ 2 (/ (* 3 (sqrt 2)) 4)))))
(deftest radius-circle-4
  (is (= (radius (matrix 1 0) [(circle [0 0] 2) (circle [0 3] 3)]) (+ (sqrt 10) 3))))
(deftest radius-circle-polygon
  (is (= (radius (matrix 1/4 1/4) [(circle [0 0] 1) (polygon [0 0] [1 0] [1 1] [0 1])]) (+ (/ (sqrt 2) 4) 1))))

(deftest body-1
  (let [result-body (body identity-transform (polygon [0 0] [1 0] [1 1] [0 1]))
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia positive-infinity
                                               :radius (/ (sqrt 2))})]
    (is (= result-body expected-body))))

(deftest body-2
  (let [result-body (body identity-transform (polygon 10 [0 0] [1 0] [1 1] [0 1]))
        expected-body (merge default-body-map {:shapes [(polygon 10 [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass 10
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia 5/3
                                               :radius (/ (sqrt 2))})]
    (is (= result-body expected-body))))
(deftest body-3
  (let [result-body (body identity-transform (circle [0 0] 1))
        expected-body (merge default-body-map {:shapes [(circle [0 0] 1)]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 0 0)
                                               :center-of-mass (matrix 0 0)
                                               :moment-of-inertia positive-infinity
                                               :radius 1})]
    (is (= result-body expected-body))))
(deftest body-4
  (let [result-body (body identity-transform (circle 10 [0 0] 1))
        expected-body (merge default-body-map {:shapes [(circle [0 0] 1)]
                                               :transform identity-transform
                                               :mass 10
                                               :center (matrix 0 0)
                                               :center-of-mass (matrix 0 0)
                                               :moment-of-inertia 5
                                               :radius 1})]
    (is (= result-body expected-body))))
(deftest body-5
  (let [result-body (body identity-transform (polygon [0 0] [1 0] [1 1] [0 1]) (polygon [1 1] [2 1] [2 2] [1 2]))
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1]) (polygon [1 1] [2 1] [2 2] [1 2])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1 1)
                                               :center-of-mass (matrix 1 1)
                                               :moment-of-inertia positive-infinity
                                               :radius (sqrt 2)})]
    (is (= result-body expected-body))))
(deftest body-6
  (let [result-body (body identity-transform (polygon [0 0] [1 0] [1 1] [0 1]) (circle [0 0] 1))
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1]) (circle [0 0] 1)]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/4 1/4)
                                               :center-of-mass (matrix 1/4 1/4)
                                               :moment-of-inertia positive-infinity
                                               :radius (+ (/ (sqrt 2) 4) 1)})]
    (is (= result-body expected-body))))
(deftest body-7
  (let [result-body (body identity-transform (circle [0 0] 1) (polygon [0 0] [1 0] [1 1] [0 1]))
        expected-body (merge default-body-map {:shapes [(circle [0 0] 1) (polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/4 1/4)
                                               :center-of-mass (matrix 1/4 1/4)
                                               :moment-of-inertia positive-infinity
                                               :radius (+ (/ (sqrt 2) 4) 1)})]
    (is (= result-body expected-body))))
(deftest body-8
  (let [result-body (body identity-transform (circle [1 0] 2) (circle [0 0] 1))
        expected-body (merge default-body-map {:shapes [(circle [1 0] 2) (circle [0 0] 1)]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 0)
                                               :center-of-mass (matrix 1/2 0)
                                               :moment-of-inertia positive-infinity
                                               :radius 5/2})]
    (is (= result-body expected-body))))
(deftest body-9
  (let [result-body (body identity-transform (polygon 1 [0 0] [1 0] [1 1] [0 1]) (polygon 2 [1 1] [2 1] [2 2] [1 2]))
        expected-body (merge default-body-map {:shapes [(polygon 1 [0 0] [1 0] [1 1] [0 1]) (polygon 2 [1 1] [2 1] [2 2] [1 2])]
                                               :transform identity-transform
                                               :mass 3
                                               :center (matrix 1 1)
                                               :center-of-mass (matrix 7/6 7/6)
                                               :moment-of-inertia 11/6
                                               :radius (sqrt 2)})]
    (is (= result-body expected-body))))
(deftest body-10
  (let [result-body (body identity-transform (polygon 5 [0 0] [1 0] [1 1] [0 1]) (circle 7 [0 0] 1))
        expected-body (merge default-body-map {:shapes [(polygon 5 [0 0] [1 0] [1 1] [0 1]) (circle 7 [0 0] 1)]
                                               :transform identity-transform
                                               :mass 12
                                               :center (matrix 1/4 1/4)
                                               :center-of-mass (matrix 5/24 5/24)
                                               :moment-of-inertia 139/24
                                               :radius (+ (/ (sqrt 2) 4) 1)})]
    (is (= result-body expected-body))))
(deftest body-11
  (let [result-body (body identity-transform (circle 10 [0 0] 1) (polygon 14 [0 0] [1 0] [1 1] [0 1]))
        expected-body (merge default-body-map {:shapes [(circle 10 [0 0] 1) (polygon 14 [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass 24
                                               :center (matrix 1/4 1/4)
                                               :center-of-mass (matrix 7/24 7/24)
                                               :moment-of-inertia 41/4
                                               :radius (+ (/ (sqrt 2) 4) 1)})]
    (is (= result-body expected-body))))
(deftest body-12
  (let [result-body (body identity-transform (circle 25 [1 0] 2) (circle 30 [0 0] 1))
        expected-body (merge default-body-map {:shapes [(circle 25 [1 0] 2) (circle 30 [0 0] 1)]
                                               :transform identity-transform
                                               :mass 55
                                               :center (matrix 1/2 0)
                                               :center-of-mass (matrix 5/11 0)
                                               :moment-of-inertia 865/11
                                               :radius 5/2})]
    (is (= result-body expected-body))))
(deftest body-13
  (let [result-body (body (polygon [0 0] [1 0] [1 1] [0 1]))
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia positive-infinity
                                               :radius (/ (sqrt 2))})]
    (is (= result-body expected-body))))
(deftest body-step-infinite-1
  (let [result-body (step (body (polygon [0 0] [1 0] [1 1] [0 1])) (matrix 0 0) 0 0)
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia positive-infinity
                                               :radius (/ (sqrt 2))
                                               :linear-momentum (matrix 0 0)
                                               :angular-momentum 0})]
    (is (= result-body expected-body))))
(deftest body-step-infinite-2
  (let [result-body (step (body (polygon [0 0] [1 0] [1 1] [0 1])) (matrix 1 1) 0 0)
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia positive-infinity
                                               :radius (/ (sqrt 2))
                                               :linear-momentum (matrix 1 1)
                                               :angular-momentum 0})]
    (is (= result-body expected-body))))
(deftest body-step-infinite-3
  (let [result-body (step (body (polygon [0 0] [1 0] [1 1] [0 1])) (matrix 0 0) 1 0)
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia positive-infinity
                                               :radius (/ (sqrt 2))
                                               :linear-momentum (matrix 0 0)
                                               :angular-momentum 1})]
    (is (= result-body expected-body))))
(deftest body-step-infinite-4
  (let [result-body (step (body (polygon [0 0] [1 0] [1 1] [0 1])) (matrix 1 1) 0 1)
        expected-body (merge default-body-map {:shapes [(polygon  [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia positive-infinity
                                               :radius (/ (sqrt 2))
                                               :linear-momentum (matrix 1 1)
                                               :angular-momentum 0})]
    (is (= result-body expected-body))))
(deftest body-step-infinite-5
  (let [result-body (step (body (polygon [0 0] [1 0] [1 1] [0 1])) (matrix 0 0) 1 1)
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia positive-infinity
                                               :radius (/ (sqrt 2))
                                               :linear-momentum (matrix 0 0)
                                               :angular-momentum 1})]
    (is (= result-body expected-body))))
(deftest body-step-infinite-6
  (let [result-body (step (body (polygon [0 0] [1 0] [1 1] [0 1])) (matrix 1 1) 1 1)
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia positive-infinity
                                               :radius (/ (sqrt 2))
                                               :linear-momentum (matrix 1 1)
                                               :angular-momentum 1})]
    (is (= result-body expected-body))))
(deftest body-step-finite-1
  (let [result-body (step (body (polygon 2 [0 0] [1 0] [1 1] [0 1])) (matrix 0 0) 0 0)
        expected-body (merge default-body-map {:shapes [(polygon 2 [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass 2
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia 1/3
                                               :radius (/ (sqrt 2))
                                               :linear-momentum (matrix 0 0)
                                               :angular-momentum 0})]
    (is (= result-body expected-body))))
(deftest body-step-finite-2
  (let [result-body (step (body (polygon 2 [0 0] [1 0] [1 1] [0 1])) (matrix 1 1) 0 0)
        expected-body (merge default-body-map {:shapes [(polygon 2 [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass 2
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia 1/3
                                               :radius (/ (sqrt 2))
                                               :linear-momentum (matrix 1 1)
                                               :angular-momentum 0})]
    (is (= result-body expected-body))))
(deftest body-step-finite-3
  (let [result-body (step (body (polygon 2 [0 0] [1 0] [1 1] [0 1])) (matrix 0 0) 1 0)
        expected-body (merge default-body-map {:shapes [(polygon 2 [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass 2
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia 1/3
                                               :radius (/ (sqrt 2))
                                               :linear-momentum (matrix 0 0)
                                               :angular-momentum 1})]
    (is (= result-body expected-body))))
(deftest body-step-finite-4
  (let [result-body (step (body (polygon 2 [0 0] [1 0] [1 1] [0 1])) (matrix 1 1) 0 1)
        expected-body (merge default-body-map {:shapes [(polygon 2 [0 0] [1 0] [1 1] [0 1])]
                                               :transform (transform 1/2 1/2 0)
                                               :mass 2
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia 1/3
                                               :radius (/ (sqrt 2))
                                               :linear-momentum (matrix 1 1)
                                               :angular-momentum 0})]
    (is (= result-body expected-body))))
(deftest body-step-finite-5
  (let [result-body (step (body (polygon 2 [0 0] [1 0] [1 1] [0 1])) (matrix 0 0) 1 1)
        expected-body (merge default-body-map {:shapes [(polygon 2 [0 0] [1 0] [1 1] [0 1])]
                                               :transform (transform (+ (matrix 1/2 1/2) (* (rotation 3) (matrix -1/2 -1/2))) 3)
                                               :mass 2
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia 1/3
                                               :radius (/ (sqrt 2))
                                               :linear-momentum (matrix 0 0)
                                               :angular-momentum 1})]
    (is (= result-body expected-body))))
(deftest body-step-finite-6
  (let [result-body (step (body (polygon 2 [0 0] [1 0] [1 1] [0 1])) (matrix 1 1) 1 1)
        expected-body (merge default-body-map {:shapes [(polygon 2 [0 0] [1 0] [1 1] [0 1])]
                                               :transform (transform (+ (matrix 1/2 1/2) (matrix 1/2 1/2) (* (rotation 3) (matrix -1/2 -1/2))) 3)
                                               :mass 2
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia 1/3
                                               :radius (/ (sqrt 2))
                                               :linear-momentum (matrix 1 1)
                                               :angular-momentum 1})]
    (is (= result-body expected-body))))

(deftest collision-equal-1
  (is (= (with-meta {:body1 (body identity-transform (polygon [0 0] [1 0] [1 1] [0 1]))
                     :body2 (body (transform 1 2 3) (polygon [1 1] [2 1] [2 2] [1 2]))
                     :normal (matrix 1 0)
                     :time 3
                     :point (matrix 1/2 1/2)
                     :face1 [(matrix 1 1) (matrix 2 2)]
                     :face2 [(matrix 3 3) (matrix 4 4)]}
           {:type :contact})
         (with-meta {:body1 (body identity-transform (polygon [0 0] [1 0] [1 1] [0 1]))
                     :body2 (body (transform 1 2 3) (polygon [1 1] [2 1] [2 2] [1 2]))
                     :normal (matrix 1 0)
                     :time 3
                     :point (matrix 1/2 1/2)
                     :face1 [(matrix 1 1) (matrix 2 2)]
                     :face2 [(matrix 3 3) (matrix 4 4)]}
           {:type :contact}))))
(deftest collision-equal-2
  (is (= (with-meta {:body1 (body identity-transform (polygon [0 0] [1 0] [1 1] [0 1]))
                     :body2 (body (transform 1 2 3) (polygon [1 1] [2 1] [2 2] [1 2]))
                     :normal (matrix 1 0)
                     :time 3
                     :point (matrix 1/2 1/2)}
           {:type :contact})
         (with-meta {:body1 (body identity-transform (polygon [0 0] [1 0] [1 1] [0 1]))
                     :body2 (body (transform 1 2 3) (polygon [1 1] [2 1] [2 2] [1 2]))
                     :normal (matrix 1 0)
                     :time 3
                     :point (matrix 1/2 1/2)}
           {:type :contact}))))

(deftest collision-1
  (let [body1 (assoc (body identity-transform (polygon 1 [0 0] [1 0] [1 1] [0 1])) :linear-momentum (matrix 1 0))
        body2 (body (transform 2 1/2 0) (polygon 1 [0 0] [1 0] [1 1] [0 1]))
        result-collision (collision body1 body2 1)
        expected-collision [{:body1 body1
                             :body2 body2
                             :normal (matrix 1 0)
                             :time 1
                             :point (matrix 2 1/2)
                             :face1 [(matrix 2 0) (matrix 2 1)]
                             :face2 [(matrix 2.0 1.5) (matrix 2.0 0.5)]}
                            {:body1 body1
                             :body2 body2
                             :normal (matrix 1 0)
                             :time 1
                             :point (matrix 2 1)
                             :face1 [(matrix 2 0) (matrix 2 1)]
                             :face2 [(matrix 2.0 1.5) (matrix 2.0 0.5)]}]]
    (is (seq= result-collision expected-collision))))

  (deftest collision-2
    (let [body1 (assoc (body identity-transform (polygon 1 [0 0] [1 0] [1 1] [0 1])) :linear-momentum (matrix 1 0))
          body2 (body (transform 2 1/2 0) (polygon 1 [0 0] [1 0] [1 1] [0 1]))
          result-collision (collision body1 body2 2)
          expected-collision [{:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 1/2)
                               :face1 [(matrix 2 0) (matrix 2 1)]
                               :face2 [(matrix 2.0 1.5) (matrix 2.0 0.5)]}
                              {:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 1)
                               :face1 [(matrix 2 0) (matrix 2 1)]
                               :face2 [(matrix 2.0 1.5) (matrix 2.0 0.5)]}]]
      (is (seq= result-collision expected-collision)))))
  (deftest collision-3
    (let [body1 (assoc (body (transform 0 1/2 0) (polygon 1 [0 0] [1 0] [1 1] [0 1])) :linear-momentum (matrix 1 0))
          body2 (body (transform 2 0 0) (polygon 1 [0 0] [1 0] [1 1] [0 1]))
          _ (collision (first (:shapes body1)) body1 (first (:shapes body2)) body2 1)
          ;result-collision (collision body1 body2 1)
          expected-collision [{:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 1/2)
                               :face1 [(matrix 2 1/2) (matrix 2 3/2)]
                               :face2 [(matrix 2 1) (matrix 2 0)]}
                              {:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 1)
                               :face1 [(matrix 2 1/2) (matrix 2 3/2)]
                               :face2 [(matrix 2 1) (matrix 2 0)]}]]
      ;(doseq [x (dissoc (first result-collision) :body1 :body2)] (println x))
      ;(doseq [x (dissoc (second result-collision) :body1 :body2)] (println x))
      ;(is (seq= result-collision expected-collision))
      ))(comment
  (deftest collision-4
    (let [body1 (assoc (body identity-transform (polygon 1 [0 0] [1 0] [1 1] [0 1])) :linear-momentum (matrix 1 0))
          body2 (body (transform 2 1/2 0) (polygon 1 [0 0] [1 0] [1 1] [0 1]))
          result-collision (collision body1 body2 2)
          expected-collision [{:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 1/2)
                               :face1 [(matrix 2 1/2) (matrix 2 3/2)]
                               :face2 [(matrix 2 1) (matrix 2 0)]}
                              {:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 1)
                               :face1 [(matrix 2 1/2) (matrix 2 3/2)]
                               :face2 [(matrix 2 1) (matrix 2 0)]}]]
      (is (seq= result-collision expected-collision))))

  (deftest collision-5
    (let [body1 (assoc (body identity-transform (polygon 1 [0 0] [1 0] [1 1] [0 1])) :linear-momentum (matrix 1 0))
          body2 (body (transform 2 1 0) (polygon 1 [0 0] [1 0] [1 1] [0 1]))
          result-collision (collision body1 body2 1)
          expected-collision [{:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 1)}]]
      (is (seq= result-collision expected-collision))))
  (deftest collision-6
    (let [body1 (assoc (body identity-transform (polygon 1 [0 0] [1 0] [1 1] [0 1])) :linear-momentum (matrix 1 0))
          body2 (body (transform 2 1 0) (polygon 1 [0 0] [1 0] [1 1] [0 1]))
          result-collision (collision body1 body2 2)
          expected-collision [{:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 1)}]]
      (is (seq= result-collision expected-collision))))
  (deftest collision-7
    (let [body1 (assoc (body (transform 0 1 0) (polygon 1 [0 0] [1 0] [1 1] [0 1])) :linear-momentum (matrix 1 0))
          body2 (body (transform 2 0 0) (polygon 1 [0 0] [1 0] [1 1] [0 1]))
          result-collision (collision body1 body2 1)
          expected-collision [{:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 1)}]]
      (is (seq= result-collision expected-collision))))
  (deftest collision-8
    (let [body1 (assoc (body (transform 0 1 0) (polygon 1 [0 0] [1 0] [1 1] [0 1])) :linear-momentum (matrix 1 0))
          body2 (body (transform 2 0 0) (polygon 1 [0 0] [1 0] [1 1] [0 1]))
          result-collision (collision body1 body2 2)
          expected-collision [{:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 1)}]]
      (is (seq= result-collision expected-collision))))
  (deftest collision-9
    (let [body1 (assoc (body identity-transform (polygon 1 [1/2 1/4] [1 1/4] [1 3/4] [1/2 3/4])) :linear-momentum (matrix 1 0))
          body2 (body (transform 2 0 0) (polygon 1 [0 0] [1 0] [1 1] [0 1]))
          result-collision (collision body1 body2 1)
          expected-collision [{:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 1/4)
                               :face1 [(matrix 2 1/4) (matrix 2 3/4)]
                               :face2 [(matrix 2 1) (matrix 2 0)]}
                              {:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 3/4)
                               :face1 [(matrix 2 1/4) (matrix 2 3/4)]
                               :face2 [(matrix 2 1) (matrix 2 0)]}]]
      (is (seq= result-collision expected-collision))))
  (deftest collision-10
    (let [body1 (assoc (body identity-transform (polygon 1 [1/2 1/4] [1 1/4] [1 3/4] [1/2 3/4])) :linear-momentum (matrix 1 0))
          body2 (body (transform 2 0 0) (polygon 1 [0 0] [1 0] [1 1] [0 1]))
          result-collision (collision body1 body2 2)
          expected-collision [{:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 1/4)
                               :face1 [(matrix 2 1/4) (matrix 2 3/4)]
                               :face2 [(matrix 2 1) (matrix 2 0)]}
                              {:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 3/4)
                               :face1 [(matrix 2 1/4) (matrix 2 3/4)]
                               :face2 [(matrix 2 1) (matrix 2 0)]}]]
      (is (seq= result-collision expected-collision))))

  (deftest collision-11
    (let [body1 (assoc (body identity-transform (polygon 1 [0 0] [1 0] [1 1] [0 1])) :linear-momentum (matrix 1 0))
          body2 (body (transform 2 0 0) (polygon 1 [0 1/4] [1/2 1/4] [1/2 3/4] [0 3/4]))
          result-collision (collision body1 body2 1)
          expected-collision [{:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 1/4)
                               :face1 [(matrix 2 0) (matrix 2 1)]
                               :face2 [(matrix 2 3/4) (matrix 2 1/4)]}
                              {:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 3/4)
                               :face1 [(matrix 2 0) (matrix 2 1)]
                               :face2 [(matrix 2 3/4) (matrix 2 1/4)]}]]
      (is (seq= result-collision expected-collision))))
  (deftest collision-12
    (let [body1 (assoc (body identity-transform (polygon 1 [1/2 1/4] [1 1/4] [1 3/4] [1/2 3/4])) :linear-momentum (matrix 1 0))
          body2 (body (transform 2 0 0) (polygon 1 [0 0] [1 0] [1 1] [0 1]))
          result-collision (collision body1 body2 2)
          expected-collision [{:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 1/4)
                               :face1 [(matrix 2 0) (matrix 2 1)]
                               :face2 [(matrix 2 3/4) (matrix 2 1/4)]}
                              {:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 3/4)
                               :face1 [(matrix 2 0) (matrix 2 1)]
                               :face2 [(matrix 2 3/4) (matrix 2 1/4)]}]]
      (is (seq= result-collision expected-collision))))
  (deftest collision-circle-1
    (let [body1 (assoc (body identity-transform (circle 1 [0 0] 1)) :linear-momentum (matrix 1 0))
          body2 (body (transform [3 0] 0) (circle 1 [0 0] 1))
          result-collision (collision body1 body2 1)
          expected-collision [{:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 0)}]]
      (is (seq= result-collision expected-collision))))
  (deftest collision-circle-2
    (let [body1 (assoc (body identity-transform (circle 1 [0 0] 1)) :linear-momentum (matrix 1 0))
          body2 (body (transform [3 0] 0) (circle 1 [0 0] 1))
          result-collision (collision body1 body2 2)
          expected-collision [{:body1 body1
                               :body2 body2
                               :normal (matrix 1 0)
                               :time 1
                               :point (matrix 2 0)}]]
      (is (seq= result-collision expected-collision))))
)
