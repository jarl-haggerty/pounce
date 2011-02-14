(comment
  Copyright 2010 Jarl Haggerty

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0
  
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.)

(ns org.curious.pounce.shape
  "Defines shape structure and functions for working with them."
  (:import java.awt.Color)
  (:require [org.curious.pounce.math.core :as math]
            [org.curious.pounce.math.matrix :as matrix]))

(defprotocol Shape
  (normals [this direction])
  (projection [this line])
  (transform [this t])
  (translate [this t])
  (rotate [this t])
  (render [this graphics]))

(def polygon)
<<<<<<< HEAD
=======
(def circle)

(defn projection= [this that] (and (math/eps= (:start this) (:start that))
                                   (math/eps= (:stop this) (:stop that))
                                   (= (:start-points this) (:start-points that))
                                   (= (:start-points this) (:start-points that))))
>>>>>>> 719c6af97bac59532bbe0c5eb1be941b74ca31d6

(defrecord Polygon [points mass center normals moment-of-inertia]
  Shape
  (normals [this direction] (filter #(math/eps<= 0 (matrix/dot direction (:normal %)))
                                    (map #(hash-map :normal %1 :side [%2 %3])
                                         normals
                                         points
                                         (conj (vec (rest points)) (first points)))))
  (projection [this line] (let [circ-points (math/circular-indexer points)
                                front-point-index (apply min-key #(matrix/dot line (circ-points %)) (range (count points)))
                                front-point (circ-points front-point-index)
                                front [(matrix/dot line front-point)
                                       (condp
                                           math/eps= (matrix/dot line front-point)
                                         (matrix/dot line (circ-points (inc front-point-index)))
                                         [front-point (circ-points (inc front-point-index))]
                                         (matrix/dot line (circ-points (dec front-point-index)))
                                         [(circ-points (dec front-point-index)) front-point]
                                         [front-point])]
                                 back-point-index (apply max-key #(matrix/dot line (circ-points %)) (range (count points)))
                                 back-point (circ-points back-point-index)
                                 back [(matrix/dot line back-point)
                                       (condp
                                           math/eps= (matrix/dot line back-point)
                                         (matrix/dot line (circ-points (inc back-point-index)))
                                         [back-point (circ-points (inc back-point-index))]
                                         (matrix/dot line (circ-points (dec back-point-index)))
                                         [(circ-points (dec back-point-index)) back-point]
                                         [back-point])]]
                             {:start (first front) :stop (first back) :start-points (second front) :stop-points (second back)}))
<<<<<<< HEAD
  (transform [this t] (Polygon. (map #(matrix/transform % t) points) mass (matrix/transform center t) (map #(matrix/mul (:rotation t) %) normals) moment-of-inertia))
;  (apply polygon mass (map #(matrix/add t %) points))
  (translate [this t] (Polygon. (vec (map #(matrix/add t %) points)) mass (matrix/add t center) normals moment-of-inertia))
  (rotate [this t] (Polygon. (vec (map #(matrix/mul t %) points)) mass (matrix/mul t center) (vec (map #(matrix/mul t %) normals)) moment-of-inertia))
=======
  (transform [this t] (apply polygon mass (map #(matrix/transform % t) points)))
  (translate [this t] (apply polygon mass (map #(matrix/add t %) points)))
  (rotate [this t] (apply polygon mass (map #(matrix/mul t %) points)))
>>>>>>> 719c6af97bac59532bbe0c5eb1be941b74ca31d6
  (render [this graphics]
          (doseq [[v1 v2] (map vector
                               points
                               (conj (vec (rest points)) (first points)))]
            (.drawLine graphics (matrix/x v1) (- (-> graphics .getClipBounds .getHeight) (matrix/y v1))
                       (matrix/x v2) (- (-> graphics .getClipBounds .getHeight) (matrix/y v2))))))

(defrecord Circle [center mass radius moment-of-inertia]
  Shape
  (normals [this direction] (let [side (matrix/mul direction radius)]
                              [{:normal direction :side [side (matrix/add side (matrix/mul (matrix/unit (matrix/rotate direction (/ math/pi 2))) math/eps))]}]))
  (projection [this line]
              {:start (- (matrix/dot center line) radius)
               :stop (+ (matrix/dot center line) radius)
               :start-points [(matrix/sub center (matrix/mul line radius))]
               :stop-points [(matrix/add center (matrix/mul line radius))]})
<<<<<<< HEAD
  (transform [this t] (Circle. (matrix/transform center t) mass radius moment-of-inertia))
  (translate [this t] (Circle. (matrix/add t center) mass radius moment-of-inertia))
  (rotate [this t] (Circle. (matrix/mul t center) mass radius moment-of-inertia))
=======
  (transform [this t] (circle mass (matrix/transform center t) radius))
  (translate [this t] (circle mass (matrix/add t center) radius))
  (rotate [this t] (circle mass (matrix/mul t center) radius))
>>>>>>> 719c6af97bac59532bbe0c5eb1be941b74ca31d6
  (render [this graphics]
          (.drawOval graphics
                     (- (matrix/x center) radius)
                     (- (matrix/y center) radius)
                     radius
                     radius)))

(defn polygon [raw-mass & raw-points]
  "Creates a shape with mass as the first argument and vetices as the rest, or, if the first argument isn't a scalar all the
   arguments will be used as vertices and the shape will have an infinite mass"
  (let [[mass points] (if (number? raw-mass)
<<<<<<< HEAD
                        [raw-mass (vec (map matrix/mat raw-points))]
                        [math/positive-infinity (vec (map matrix/mat (cons raw-mass raw-points)))])
=======
                        [raw-mass (vec (map matrix/create raw-points))]
                        [math/positive-infinity (vec (map matrix/create (cons raw-mass raw-points)))])
>>>>>>> 719c6af97bac59532bbe0c5eb1be941b74ca31d6
        center (matrix/div (reduce matrix/add points) (count points))
        sides (map #(vector %1 %2) points (conj (vec (rest points)) (first points)))
        moment-of-inertia (* mass 1/6
                             (reduce /
                                     (reduce #(map + %1 %2)
                                             (for [pair (map #(vector (matrix/sub (first %) center) (matrix/sub (second %) center)) sides)]
                                               (let [a (matrix/cross (first pair) (second pair))
                                                     b (+ (matrix/dot (first pair) (first pair))
                                                          (matrix/dot (first pair) (second pair))
                                                          (matrix/dot (second pair) (second pair)))]
                                                 [(* a b) a])))))]
    (Polygon. points
              mass
              center
              (map #(-> (matrix/sub (second %) (first %)) 
                        (matrix/rotate (/ math/pi -2))
                        matrix/unit)
                   sides)
              moment-of-inertia)))

(defn circle
  "Creates a circle with the specified mass or inifinite mass if not defined and with the
   specified center and radius."
  ([center radius] (circle math/positive-infinity center radius))
  ([mass center radius]
<<<<<<< HEAD
     (Circle. (matrix/mat center)
=======
     (Circle. (matrix/create center)
>>>>>>> 719c6af97bac59532bbe0c5eb1be941b74ca31d6
              mass
              radius
              (* mass radius radius 1/2))))
