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
  (let [result-body (body identity-transform (polygon [0 0] [0 1] [1 0] [1 1]) (circle [0 0] 1))
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [0 1] [1 0] [1 1]) (circle [0 0] 1)]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/4 1/4)
                                               :center-of-mass (matrix 1/4 1/4)
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
  (let [result-body (body identity-transform (circle [0 0] 1) (polygon [0 0] [0 1] [1 0] [1 1]))
        expected-body (merge default-body-map {:shapes [(circle [0 0] 1) (polygon [0 0] [0 1] [1 0] [1 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/4 1/4)
                                               :center-of-mass (matrix 1/4 1/4)
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
  (let [result-body (body identity-transform (polygon 5 [0 0] [0 1] [1 0] [1 1]) (circle 7 [0 0] 1))
        expected-body (merge default-body-map {:shapes [(polygon 5 [0 0] [0 1] [1 0] [1 1]) (circle 7 [0 0] 1)]
                                               :transform identity-transform
                                               :mass 12
                                               :center (matrix 1/4 1/4)
                                               :center-of-mass (matrix 5/24 5/24)
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
  (let [result-body (body identity-transform (circle 10 [0 0] 1) (polygon 14 [0 0] [0 1] [1 0] [1 1]))
        expected-body (merge default-body-map {:shapes [(circle 10 [0 0] 1) (polygon 14 [0 0] [0 1] [1 0] [1 1])]
                                               :transform identity-transform
                                               :mass 24
                                               :center (matrix 1/4 1/4)
                                               :center-of-mass (matrix 7/24 7/24)
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

(deftest body-step-1
  (let [result-body (step (body (polygon [0 0] [1 0] [1 1] [0 1])) (matrix 0 0) 0 0)
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
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
(deftest body-step-2
  (let [result-body (step (body (polygon [0 0] [1 0] [1 1] [0 1])) (matrix 1 1) 0 0)
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
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
(deftest body-step-3
  (let [result-body (step (body (polygon [0 0] [1 0] [1 1] [0 1])) (matrix 0 0) 1 0)
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
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
(deftest body-step-4
  (let [result-body (step (body (polygon [0 0] [1 0] [1 1] [0 1])) (matrix 1 1) 0 1)
        expected-body (merge default-body-map {:shapes [(polygon  [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
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
(deftest body-step-5
  (let [result-body (step (body (polygon [0 0] [1 0] [1 1] [0 1])) (matrix 0 0) 1 1)
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
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
(deftest body-step-6
  (let [result-body (step (body (polygon [0 0] [1 0] [1 1] [0 1])) (matrix 1 1) 1 1)
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
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

(deftest body-step-1
  (let [result-body (step (body (polygon 2 [0 0] [1 0] [1 1] [0 1])) (matrix 0 0) 0 0)
        expected-body (merge default-body-map {:shapes [(polygon 2 [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
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
(deftest body-step-2
  (let [result-body (step (body (polygon 2 [0 0] [1 0] [1 1] [0 1])) (matrix 1 1) 0 0)
        expected-body (merge default-body-map {:shapes [(polygon 2 [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
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
(deftest body-step-3
  (let [result-body (step (body (polygon 2 [0 0] [1 0] [1 1] [0 1])) (matrix 0 0) 1 0)
        expected-body (merge default-body-map {:shapes [(polygon 2 [0 0] [1 0] [1 1] [0 1])]
                                               :transform identity-transform
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
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
(deftest body-step-4
  (let [result-body (step (body (polygon 2 [0 0] [1 0] [1 1] [0 1])) (matrix 1 1) 0 1)
        expected-body (merge default-body-map {:shapes [(polygon 2 [0 0] [1 0] [1 1] [0 1])]
                                               :transform (transform 1/2 1/2 0)
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
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
(deftest body-step-5
  (let [result-body (step (body (polygon 2 [0 0] [1 0] [1 1] [0 1])) (matrix 0 0) 1 1)
        expected-body (merge default-body-map {:shapes [(polygon 2 [0 0] [1 0] [1 1] [0 1])]
                                               :transform (transform (+ (matrix 1/2 1/2) (* (rotation 1) (matrix -1/2 -1/2))) 1)
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
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
(deftest body-step-6
  (let [result-body (step (body (polygon [0 0] [1 0] [1 1] [0 1])) (matrix 1 1) 1 1)
        expected-body (merge default-body-map {:shapes [(polygon [0 0] [1 0] [1 1] [0 1])]
                                               :transform (transform (+ (matrix 1 1) (matrix 1/2 1/2) (* (rotation 1) (matrix -1/2 -1/2))) 1)
                                               :mass positive-infinity
                                               :center (matrix 1/2 1/2)
                                               :center-of-mass (matrix 1/2 1/2)
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


