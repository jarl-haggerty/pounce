(ns pounce.test.body
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:use pounce.body
        pounce.math.math
        pounce.math.matrix
        pounce.shape
        clojure.test
        clojure.set))

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
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
(deftest body-2
  (let [result-body (body identity-transform (polygon 10 [0 0] [1 0] [1 1] [0 1]))
        expected-body (merge default-body-map {:shapes [(polygon 10 [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass 10
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia 5/3
                                               :radius (/ (sqrt 2))})]
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
(deftest body-3
  (let [result-body (body identity-transform (circle [0 0] 1))
        expected-body (merge default-body-map {:shapes [(circle [0 0] 1)]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 0 0)
                                               :center-of-mass (matrix 0 0)
                                               :moment-of-inertia positive-infinity
                                               :radius 1})]
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
(deftest body-4
  (let [result-body (body identity-transform (circle 10 [0 0] 1))
        expected-body (merge default-body-map {:shapes [(circle [0 0] 1)]
                                               :transform identity-transform
                                               :mass 10
                                               :center (matrix 0 0)
                                               :center-of-mass (matrix 0 0)
                                               :moment-of-inertia 5
                                               :radius 1})]
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
(deftest body-5
  (let [result-body (body identity-transform (polygon [0 0] [1 0] [1 1] [0 1]) (polygon [1 1] [2 1] [2 2] [1 2]))
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1]) (polygon [1 1] [2 1] [2 2] [1 2])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1 1)
                                               :center-of-mass (matrix 1 1)
                                               :moment-of-inertia positive-infinity
                                               :radius (sqrt 2)})]
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
(deftest body-6
  (let [result-body (body identity-transform (polygon [0 0] [1 0] [1 1] [0 1]) (circle [0 0] 1))
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1]) (circle [0 0] 1)]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/4 1/4)
                                               :center-of-mass (matrix 1/4 1/4)
                                               :moment-of-inertia positive-infinity
                                               :radius (+ (/ (sqrt 2) 4) 1)})]
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
(deftest body-7
  (let [result-body (body identity-transform (circle [0 0] 1) (polygon [0 0] [1 0] [1 1] [0 1]))
        expected-body (merge default-body-map {:shapes [(circle [0 0] 1) (polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/4 1/4)
                                               :center-of-mass (matrix 1/4 1/4)
                                               :moment-of-inertia positive-infinity
                                               :radius (+ (/ (sqrt 2) 4) 1)})]
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body) (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
(deftest body-8
  (let [result-body (body identity-transform (circle [1 0] 2) (circle [0 0] 1))
        expected-body (merge default-body-map {:shapes [(circle [1 0] 2) (circle [0 0] 1)]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 0)
                                               :center-of-mass (matrix 1/2 0)
                                               :moment-of-inertia positive-infinity
                                               :radius 5/2})]
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
(deftest body-9
  (let [result-body (body identity-transform (polygon 1 [0 0] [1 0] [1 1] [0 1]) (polygon 2 [1 1] [2 1] [2 2] [1 2]))
        expected-body (merge default-body-map {:shapes [(polygon 1 [0 0] [1 0] [1 1] [0 1]) (polygon 2 [1 1] [2 1] [2 2] [1 2])]
                                               :transform identity-transform
                                               :mass 3
                                               :center (matrix 1 1)
                                               :center-of-mass (matrix 7/6 7/6)
                                               :moment-of-inertia 11/6
                                               :radius (sqrt 2)})]
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
(deftest body-10
  (let [result-body (body identity-transform (polygon 5 [0 0] [1 0] [1 1] [0 1]) (circle 7 [0 0] 1))
        expected-body (merge default-body-map {:shapes [(polygon 5 [0 0] [1 0] [1 1] [0 1]) (circle 7 [0 0] 1)]
                                               :transform identity-transform
                                               :mass 12
                                               :center (matrix 1/4 1/4)
                                               :center-of-mass (matrix 5/24 5/24)
                                               :moment-of-inertia 139/24
                                               :radius (+ (/ (sqrt 2) 4) 1)})]
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
(deftest body-11
  (let [result-body (body identity-transform (circle 10 [0 0] 1) (polygon 14 [0 0] [1 0] [1 1] [0 1]))
        expected-body (merge default-body-map {:shapes [(circle 10 [0 0] 1) (polygon 14 [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass 24
                                               :center (matrix 1/4 1/4)
                                               :center-of-mass (matrix 7/24 7/24)
                                               :moment-of-inertia 41/4
                                               :radius (+ (/ (sqrt 2) 4) 1)})]
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
(deftest body-12
  (let [result-body (body identity-transform (circle 25 [1 0] 2) (circle 30 [0 0] 1))
        expected-body (merge default-body-map {:shapes [(circle 25 [1 0] 2) (circle 30 [0 0] 1)]
                                               :transform identity-transform
                                               :mass 55
                                               :center (matrix 1/2 0)
                                               :center-of-mass (matrix 5/11 0)
                                               :moment-of-inertia 865/11
                                               :radius 5/2})]
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
(deftest body-13
  (let [result-body (body (polygon [0 0] [1 0] [1 1] [0 1]))
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
                                               :moment-of-inertia positive-infinity
                                               :radius (/ (sqrt 2))})]
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
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
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
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
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
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
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
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
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
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
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
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
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
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
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
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
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
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
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
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
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
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
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))
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
    (is (= (:transform result-body) (:transform expected-body)))
    (is (seq= (:shapes result-body) (:shapes expected-body)))
    (is (= (:moment-of-inertia result-body) (:moment-of-inertia expected-body)))
    (is (= (:linear-momentum result-body) (:linear-momentum expected-body)))
    (is (= (:angular-momentum result-body)  (:angular-momentum expected-body)))
    (is (= (:linear-velocity result-body)  (:linear-velocity expected-body)))
    (is (= (:angular-velocity result-body)  (:angular-velocity expected-body)))
    (is (= (:mass result-body)  (:mass expected-body)))
    (is (= (:center-of-mass result-body)  (:center-of-mass expected-body)))
    (is (= (:center result-body)  (:center expected-body)))
    (is (= (:radius result-body)  (:radius expected-body)))))

(deftest collision-1
  (let [body1 (assoc (body identity-transform (polygon 1 [0 0] [1 0] [1 1] [0 1])) :linear-momentum (matrix 1 0))
        body2 (body (transform 2 1/2 0) (polygon 1 [0 0] [1 0] [1 1] [0 1]))
        time-step 2
        result-collision (collision body1 body2 time-step)
        expected-collision [{:body1 body1
                             :body2 body2
                             :normal (matrix 1 0)
                             :time 1
                             :point (matrix 2 1/2)
                             :face1 [(matrix 1 0) (matrix 1 1)]
                             :face2 [(matrix 2.0 1.5) (matrix 2.0 0.5)]}
                            {:body1 body1
                             :body2 body2
                             :normal (matrix 1 0)
                             :time 1
                             :point (matrix 2 1)
                             :face1 [(matrix 1 0) (matrix 1 1)]
                             :face2 [(matrix 2.0 1.5) (matrix 2.0 0.5)]}]]
    (is (= (count result-collision) (count expected-collision)))
    (is (= (:body1 (first result-collision)) (:body1 (first expected-collision))))
    (is (= (:body2 (first result-collision)) (:body2 (first expected-collision))))
    (is (= (:normal (first result-collision)) (:normal (first expected-collision))))
    (is (= (:time (first result-collision)) (:time (first expected-collision))))
    (is (= (:point (first result-collision)) (:point (first expected-collision))))
    (is (seq= (:face1 (first result-collision)) (:face1 (first expected-collision))))
    (is (seq= (:face2 (first result-collision)) (:face2 (first expected-collision))))
    
    (is (= (:body1 (second result-collision)) (:body1 (second expected-collision))))
    (is (= (:body2 (second result-collision)) (:body2 (second expected-collision))))
    (is (= (:normal (second result-collision)) (:normal (second expected-collision))))
    (is (= (:time (second result-collision)) (:time (second expected-collision))))
    (is (= (:point (second result-collision)) (:point (second expected-collision))))
    (is (seq= (:face1 (second result-collision)) (:face1 (second expected-collision))))
    (is (seq= (:face2 (second result-collision)) (:face2 (second expected-collision))))))
