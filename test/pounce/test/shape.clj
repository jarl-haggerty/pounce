(ns pounce.test.shape
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:use pounce.shape
        pounce.math.math
        pounce.math.matrix
        pounce.render
        clojure.test))

(deftest shape-polygon-1
  (is (= (apply polygon (map matrix [[0 0] [1 0] [1 1] [0 1]]))
         {:center {:data [1/2 1/2] :height 2 :width 1}
          :mass positive-infinity
          :points (map matrix [[0 0] [1 0] [1 1] [0 1]])
          :normals (map matrix [[0 -1] [1 0] [0 1] [-1 0]])})))
(deftest shape-polygon-2
  (is (= (apply polygon 10 (map matrix [[0 0] [1 0] [1 1] [0 1]]))
         {:center {:data [1/2 1/2] :height 2 :width 1}
          :mass 10
          :points (map matrix [[0 0] [1 0] [1 1] [0 1]])
          :normals (map matrix [[0 -1] [1 0] [0 1] [-1 0]])})))
(deftest shape-circle-1
  (is (= (circle (matrix 0 0) 5) {:mass positive-infinity :center (matrix 0 0) :radius 5})))
(deftest shape-circle-2
  (is (= (circle 10 (matrix 0 0) 5) {:mass 10 :center (matrix 0 0) :radius 5})))
(deftest normals-polygon-undirected
  (let [result-normals (normals (apply polygon (map matrix [[0 0] [1 0] [1 1] [0 1]])))
        expected-normals [{:normal (matrix 0 -1) :side [(matrix 0 0) (matrix 1 0)]}
                          {:normal (matrix 1 0) :side [(matrix 1 0) (matrix 1 1)]}
                          {:normal (matrix 0 1) :side [(matrix 1 1) (matrix 0 1)]}
                          {:normal (matrix -1 0) :side [(matrix 0 1) (matrix 0 0)]}]]
    (is (= (count result-normals) (count expected-normals) 4))
    (is (= (nth result-normals 0) (nth expected-normals 0)))
    (is (= (nth result-normals 1) (nth expected-normals 1)))
    (is (= (nth result-normals 2) (nth expected-normals 2)))
    (is (= (nth result-normals 3) (nth expected-normals 3)))))
(deftest normals-polygon-directed-1
  (let [result-normals (normals (apply polygon (map matrix [[0 0] [1 0] [1 1] [0 1]])) (matrix 1 0))
        expected-normals [{:normal (matrix 0 -1) :side [(matrix 0 0) (matrix 1 0)]}
                          {:normal (matrix 1 0) :side [(matrix 1 0) (matrix 1 1)]}
                          {:normal (matrix 0 1) :side [(matrix 1 1) (matrix 0 1)]}]]
    (is (= (count result-normals) (count expected-normals) 3))
    (is (= (nth result-normals 0) (nth expected-normals 0)))
    (is (= (nth result-normals 1) (nth expected-normals 1)))
    (is (= (nth result-normals 2) (nth expected-normals 2)))))
(deftest normals-polygon-directed-2
  (let [result-normals (normals (apply polygon (map matrix [[0 0] [1 0] [1 1] [0 1]])) (matrix 1 1))
        expected-normals [{:normal (matrix 1 0) :side [(matrix 1 0) (matrix 1 1)]}
                          {:normal (matrix 0 1) :side [(matrix 1 1) (matrix 0 1)]}]]
    (is (= (count result-normals) (count expected-normals) 2))
    (is (= (nth result-normals 0) (nth expected-normals 0)))
    (is (= (nth result-normals 1) (nth expected-normals 1)))))
(deftest normals-circle-1
  (let [result-normals (normals (circle (matrix 0 0) 5) (matrix 1 0))
        expected-normals [{:normal (matrix 1 0) :side [(matrix 5 0) (matrix 5 0)]}]]
    (is (= (count result-normals) (count expected-normals) 1))
    (is (= (nth result-normals 0) (nth expected-normals 0)))))
(deftest normals-circle-2
  (let [result-normals (normals (circle (matrix 0 0) 5) (matrix 1 1))
        expected-normals [{:normal (matrix (/ (sqrt 2)) (/ (sqrt 2))) :side [(* 5 (matrix (/ (sqrt 2)) (/ (sqrt 2)))) (* 5 (matrix (/ (sqrt 2)) (/ (sqrt 2))))]}]]
    (is (= (count result-normals) (count expected-normals) 1))
    (is (= (nth result-normals 0) (nth expected-normals 0)))))
(deftest projection-square-1
  (let [result-projection (projection (apply polygon (map matrix [[0 0] [1 0] [1 1] [0 1]])) (matrix 1 0))
        expected-projection {:start 0 :stop 1 :start-points [(matrix 0 1) (matrix 0 0)] :stop-points [(matrix 1 0) (matrix 1 1)]}]
    (is (= (:start result-projection) (:start expected-projection)))
    (is (= (:stop result-projection) (:stop expected-projection)))
    (is (seq= (:start-points result-projection) (:start-points expected-projection)))
    (is (seq= (:stop-points result-projection) (:stop-points expected-projection)))))
(deftest projection-square-2
  (let [result-projection (projection (apply polygon (map matrix [[0 0] [1 0] [1 1] [0 1]])) (matrix (/ (sqrt 2)) (/ (sqrt 2))))
        expected-projection {:start 0 :stop (sqrt 2) :start-points [(matrix 0 0)] :stop-points [(matrix 1 1)]}]
    (is (= (:start result-projection) (:start expected-projection)))
    (is (= (:stop result-projection) (:stop expected-projection)))
    (is (seq= (:start-points result-projection) (:start-points expected-projection)))
    (is (seq= (:stop-points result-projection) (:stop-points expected-projection)))))
(deftest projection-square-3
  (let [result-projection (projection (apply polygon (map matrix [[1 1] [2 1] [2 2] [1 2]])) (matrix 1 0))
        expected-projection {:start 1 :stop 2 :start-points [(matrix 1 2) (matrix 1 1)] :stop-points [(matrix 2 1) (matrix 2 2)]}]
    (is (= (:start result-projection) (:start expected-projection)))
    (is (= (:stop result-projection) (:stop expected-projection)))
    (is (seq= (:start-points result-projection) (:start-points expected-projection)))
    (is (seq= (:stop-points result-projection) (:stop-points expected-projection)))))
(deftest projection-square-4
  (let [result-projection (projection (apply polygon (map matrix [[1 1] [2 1] [2 2] [1 2]])) (matrix (/ (sqrt 2)) (/ (sqrt 2))))
        expected-projection {:start (sqrt 2) :stop (* 2 (sqrt 2)) :start-points [(matrix 1 1)] :stop-points [(matrix 2 2)]}]
    (is (= (:start result-projection) (:start expected-projection)))
    (is (= (:stop result-projection) (:stop expected-projection)))
    (is (seq= (:start-points result-projection) (:start-points expected-projection)))
    (is (seq= (:stop-points result-projection) (:stop-points expected-projection)))))
(deftest projection-octagon-1
  (let [result-projection (projection (apply polygon (map matrix [[0 0] [0.5 -0.25] [1 0] [1.25 0.5] [1 1] [0.5 1.25] [0 1] [-0.25 0.5]])) (matrix 1 0))
        expected-projection {:start -0.25 :stop 1.25 :start-points [(matrix -0.25 0.5)] :stop-points [(matrix 1.25 0.5)]}]
    (is (= (:start result-projection) (:start expected-projection)))
    (is (= (:stop result-projection) (:stop expected-projection)))
    (is (seq= (:start-points result-projection) (:start-points expected-projection)))
    (is (seq= (:stop-points result-projection) (:stop-points expected-projection)))))
(deftest projection-octagon-2
  (let [result-projection (projection (apply polygon (map matrix [[0 0] [0.5 -0.25] [1 0] [1.25 0.5] [1 1] [0.5 1.25] [0 1] [-0.25 0.5]])) (matrix (/ (sqrt 2)) (/ (sqrt 2))))
        expected-projection {:start 0 :stop (sqrt 2) :start-points [(matrix 0 0)] :stop-points [(matrix 1 1)]}]
    (is (= (:start result-projection) (:start expected-projection)))
    (is (= (:stop result-projection) (:stop expected-projection)))
    (is (seq= (:start-points result-projection) (:start-points expected-projection)))
    (is (seq= (:stop-points result-projection) (:stop-points expected-projection)))))
(deftest projection-octagon-3
  (let [result-projection (projection (apply polygon (map matrix [[1 1] [1.5 0.75] [2 1] [2.25 1.5] [2 2] [1.5 2.25] [1 2] [0.75 1.5]])) (matrix 1 0))
        expected-projection {:start 0.75 :stop 2.25 :start-points [(matrix 0.75 1.5)] :stop-points [(matrix 2.25 1.5)]}]
    (is (= (:start result-projection) (:start expected-projection)))
    (is (= (:stop result-projection) (:stop expected-projection)))
    (is (seq= (:start-points result-projection) (:start-points expected-projection)))
    (is (seq= (:stop-points result-projection) (:stop-points expected-projection)))))
(deftest projection-octagon-4
  (let [result-projection (projection (apply polygon (map matrix [[1 1] [1.5 0.75] [2 1] [2.25 1.5] [2 2] [1.5 2.25] [1 2] [0.75 1.5]])) (matrix (/ (sqrt 2)) (/ (sqrt 2))))
        expected-projection {:start (sqrt 2) :stop (* 2 (sqrt 2)) :start-points [(matrix 1 1)] :stop-points [(matrix 2 2)]}]
    (is (= (:start result-projection) (:start expected-projection)))
    (is (= (:stop result-projection) (:stop expected-projection)))
    (is (seq= (:start-points result-projection) (:start-points expected-projection)))
    (is (seq= (:stop-points result-projection) (:stop-points expected-projection)))))
(deftest projection-circle-1
  (let [result-projection (projection (circle (matrix 0 0) 2) (matrix 1 0))
        expected-projection {:start -2 :stop 2 :start-points [(matrix -2 0)] :stop-points [(matrix 2 0)]}]
    (is (= (:start result-projection) (:start expected-projection)))
    (is (= (:stop result-projection) (:stop expected-projection)))
    (is (seq= (:start-points result-projection) (:start-points expected-projection)))
    (is (seq= (:stop-points result-projection) (:stop-points expected-projection)))))
(deftest projection-circle-2
  (let [result-projection (projection (circle (matrix 0 0) 2) (matrix (/ (sqrt 2)) (/ (sqrt 2))))
        expected-projection {:start -2 :stop 2 :start-points [(* -2 (matrix (/ (sqrt 2)) (/ (sqrt 2))))] :stop-points [(* 2 (matrix (/ (sqrt 2)) (/ (sqrt 2))))]}]
    (is (= (:start result-projection) (:start expected-projection)))
    (is (= (:stop result-projection) (:stop expected-projection)))
    (is (seq= (:start-points result-projection) (:start-points expected-projection)))
    (is (seq= (:stop-points result-projection) (:stop-points expected-projection)))))
(deftest projection-circle-3
  (let [result-projection (projection (circle (matrix 4 4) 2) (matrix 1 0))
        expected-projection {:start 2 :stop 6 :start-points [(matrix 2 4)] :stop-points [(matrix 6 4)]}]
    (is (= (:start result-projection) (:start expected-projection)))
    (is (= (:stop result-projection) (:stop expected-projection)))
    (is (seq= (:start-points result-projection) (:start-points expected-projection)))
    (is (seq= (:stop-points result-projection) (:stop-points expected-projection)))))
(deftest projection-circle-4
  (let [result-projection (projection (circle (matrix 4 4) 2) (matrix (/ (sqrt 2)) (/ (sqrt 2))))
        expected-projection {:start (- (* 4 (sqrt 2)) 2)
                             :stop (+ (* 4 (sqrt 2)) 2)
                             :start-points [(+ (matrix 4 4) (* -2 (matrix (/ (sqrt 2)) (/ (sqrt 2)))))]
                             :stop-points [(+ (matrix 4 4) (* 2 (matrix (/ (sqrt 2)) (/ (sqrt 2)))))]}]
    (is (= (:start result-projection) (:start expected-projection)))
    (is (= (:stop result-projection) (:stop expected-projection)))
    (is (seq= (:start-points result-projection) (:start-points expected-projection)))
    (is (seq= (:stop-points result-projection) (:stop-points expected-projection)))))
(deftest farside-distance-polygon-1
  (is (= (farside-distance (apply polygon (map matrix [[0 0] [1 0] [1 1] [0 1]])) (matrix 0 0)) (sqrt 2))))
(deftest farside-distance-polygon-2
  (is (= (farside-distance (apply polygon (map matrix [[0 0] [1 0] [1 1] [0 1]])) (matrix 1/2 1/2)) (/ (sqrt 2) 2))))
(deftest farside-distance-circle-1
  (is (= (farside-distance (circle (matrix 0 0) 3) (matrix 0 0)) 3)))
(deftest farside-distance-circle-2
  (is (= (farside-distance (circle (matrix 0 0) 3) (matrix 1 1)) (+ 3 (sqrt 2)))))
(deftest farside-distance-circle-3
  (is (= (farside-distance (circle (matrix 1 1) 3) (matrix 0 0)) (+ 3 (sqrt 2)))))
(deftest shape-polygon-=
  (is (= (apply polygon (map matrix [[1 2] [2 2] [2 3] [1 3]])) (apply polygon (map matrix [[1 2] [2 2] [2 3] [1 3]]))))
  (is (= (apply polygon 3 (map matrix [[1 2] [2 2] [2 3] [1 3]])) (apply polygon 3 (map matrix [[1 2] [2 2] [2 3] [1 3]]))))
  (is (not= (apply polygon 3 (map matrix [[1 2] [2 2] [2 3] [1 3]])) (apply polygon 5 (map matrix [[1 2] [2 2] [2 3] [1 3]]))))
  (is (not= (apply polygon (map matrix [[1 2] [2 2] [2 3] [1 3]])) (apply polygon (map matrix [[1 1] [2 2] [2 3] [1 3]]))))
  (is (not= (apply polygon (map matrix [[1 2] [2 2] [2 3] [1 3]])) (apply polygon (map matrix [[2 2] [2 3] [1 3]])))))
(deftest shape-circle-=
  (is (= (circle (matrix 1 1) 3) (circle (matrix 1 1) 3)))
  (is (= (circle 2 (matrix 1 1) 3) (circle 2 (matrix 1 1) 3)))
  (is (not= (circle 10 (matrix 2 1) 3) (circle 2 (matrix 1 1) 3)))
  (is (not= (circle (matrix 2 1) 3) (circle (matrix 1 1) 3)))
  (is (not= (circle (matrix 1 1) 1) (circle (matrix 1 1) 3))))
(deftest shape-multiply-polygon-1
  (let [result-shape (* (transform 1 2 0) (apply polygon (map matrix [[0 0] [1 0] [1 1] [0 1]])))
        expected-shape (apply polygon (map matrix [[1 2] [2 2] [2 3] [1 3]]))]
    (is (= (:center result-shape) (:center expected-shape)))
    (is (= (:mass result-shape) (:mass expected-shape)))
    (is (seq= (:points result-shape) (:points expected-shape)))
    (is (seq= (:normals result-shape) (:normals expected-shape)))))
(deftest shape-multiply-polygon-2
  (let [result-shape (* (transform 1 2 (/ pi 4)) (apply polygon (map matrix [[0 0] [1 0] [1 1] [0 1]])))
        expected-shape (apply polygon (map #(+ (matrix 1 2) (* (rotation (/ pi 4)) (matrix %))) [[0 0] [1 0] [1 1] [0 1]]))]
    (is (= (:center result-shape) (:center expected-shape)))
    (is (= (:mass result-shape) (:mass expected-shape)))
    (is (seq= (:points result-shape) (:points expected-shape)))
    (is (seq= (:normals result-shape) (:normals expected-shape)))))
(deftest shape-multiply-polygon-3
  (let [result-shape (* (transform 0 0 (/ pi 4)) (apply polygon (map matrix [[0 0] [1 0] [1 1] [0 1]])))
        expected-shape (apply polygon (map #(* (rotation (/ pi 4)) (matrix %)) [[0 0] [1 0] [1 1] [0 1]]))]
    (is (= (:center result-shape) (:center expected-shape)))
    (is (= (:mass result-shape) (:mass expected-shape)))
    (is (seq= (:points result-shape) (:points expected-shape)))
    (is (seq= (:normals result-shape) (:normals expected-shape)))))
(deftest shape-multiply-circle-1
  (let [result-shape (* (transform 1 2 0) (circle (matrix 1 1) 2))
        expected-shape (circle (+ (matrix 1 2) (matrix 1 1)) 2)]
    (is (= (:center result-shape) (:center expected-shape)))
    (is (= (:mass result-shape) (:mass expected-shape)))
    (is (= (:radius result-shape) (:radius expected-shape)))))
(deftest shape-multiply-circle-2
  (let [result-shape (* (transform 0 0 (/ pi 4)) (circle (matrix 1 1) 2))
        expected-shape (circle (* (rotation (/ pi 4)) (matrix 1 1)) 2)]
    (is (= (:center result-shape) (:center expected-shape)))
    (is (= (:mass result-shape) (:mass expected-shape)))
    (is (= (:radius result-shape) (:radius expected-shape)))))
(deftest shape-multiply-circle-3
  (let [result-shape (* (transform 1 2 (/ pi 4)) (circle (matrix 1 1) 2))
        expected-shape (circle (+ (matrix 1 2) (* (rotation (/ pi 4)) (matrix 1 1))) 2)]
    (is (= (:center result-shape) (:center expected-shape)))
    (is (= (:mass result-shape) (:mass expected-shape)))
    (is (= (:radius result-shape) (:radius expected-shape)))))
