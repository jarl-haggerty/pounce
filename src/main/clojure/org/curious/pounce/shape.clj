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
            [org.curious.pounce.math.matrix :as matrix]
            [org.curious.pounce.render :as render]))

(defprotocol Shape
  (normals [this direction])
  (projection [this line])
  (transform [this t])
  (translate [this t])
  (rotate [this t]))

(def polygon)
(def circle)

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
  (transform [this t] (apply polygon mass (map #(matrix/transform % t) points)))
  (translate [this t] (apply polygon mass (map #(matrix/add t %) points)))
  (rotate [this t] (apply polygon mass (map #(matrix/mul t %) points)))
  render/Renderable
  (render [this graphics]
          (.setColor graphics Color/white)
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
  (transform [this t] (circle mass (matrix/transform center t) radius))
  (translate [this t] (circle mass (matrix/add t center) radius))
  (rotate [this t] (circle mass (matrix/mul t center) radius))
  render/Renderable
  (render [this graphics]
          (.setColor graphics Color/white)
          (.drawOval graphics
                     (- (matrix/x center) radius)
                     (- (matrix/y center) radius)
                     radius
                     radius)))

(defn polygon [raw-mass & raw-points]
  "Creates a shape with mass as the first argument and vetices as the rest, or, if the first argument isn't a scalar all the
   arguments will be used as vertices and the shape will have an infinite mass"
  (let [[mass points] (if (number? raw-mass)
                        [raw-mass (vec (map matrix/create raw-points))]
                        [math/positive-infinity (vec (map matrix/create (cons raw-mass raw-points)))])
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
     (Circle. (matrix/create center)
              mass
              radius
              (* mass radius radius 1/2))))

(defn collision= [this that]
  (and (math/eps= (:depth this) (:depth that))
       (= (dissoc this :depth) (dissoc that :depth))))

(defn collision
  "Calculates all the points of collisions that will occur in the next delta seconds between two bodies, neglecting rotation."
  ([shape1 shape2]
     (if-let [direction1 (collision shape1 shape2 (matrix/sub (:center shape2) (:center shape1)))]
       (if-let [direction2 (collision shape2 shape1 (matrix/sub (:center shape1) (:center shape2)))]
         (if (math/eps<= (-> direction1 first :depth)
                         (-> direction2 first :depth))
           direction1
           direction2)
         nil)
       nil))
  ([shape1 shape2 direction]
     (loop [n-stack (normals shape1 direction) accum [{:depth math/positive-infinity}]]
       (if-let [n (first n-stack)]
         (let [proj (projection (translate shape2 (-> n :side first matrix/sub)) (:normal n))
               start-points (:start-points proj)]
           (if (math/eps> (:start proj) 0)
             nil
             (let [depth (- (:start proj))
                   default-contact {:normal (:normal n) :depth depth}]
               (cond
                (math/eps<= (:depth (first accum)) depth)
                (recur (rest n-stack) accum)
                (= (count start-points) 1)
                (let [plane (matrix/unit (matrix/sub (second (:side n))
                                                     (first (:side n))))
                      side {:min 0 :min-point (first (:side n))
                            :max (matrix/length (matrix/sub (second (:side n)) (first (:side n)))) :max-point (second (:side n))}
                      point (matrix/dot plane (first start-points))]
                  (if (and (math/eps< (:min side) point) (math/eps< point (:max side)))
                    (recur (rest n-stack) [(merge default-contact {:point (matrix/add (matrix/mul plane point) (first (:side n)))})])
                    (recur (rest n-stack) accum)))
                :else
                (let [plane (matrix/unit (matrix/sub (second (:side n))
                                                     (first (:side n))))
                      side1 {:min 0 :min-point (first (:side n))
                             :max (matrix/length (matrix/sub (second (:side n)) (first (:side n)))) :max-point (second (:side n))}
                      side2 (let [first-dot (matrix/dot plane (first start-points))
                                  second-dot (matrix/dot plane (second start-points))
                                  temp (sort-by first
                                                [[first-dot (matrix/add (matrix/mul plane first-dot) (first (:side n)))]
                                                 [second-dot (matrix/add (matrix/mul plane second-dot) (first (:side n)))]])]
                              {:min (first (first temp)) :min-point (second (first temp))
                               :max (first (second temp)) :max-point (second (second temp))})]
                  (cond
                   (or (and (math/eps< (:min side1) (:min side2)) (math/eps< (:max side2) (:max side1)))
                       (and (math/eps= (:min side1) (:min side2)) (math/eps< (:max side2) (:max side1)))
                       (and (math/eps< (:min side1) (:min side2)) (math/eps= (:max side2) (:max side1))))
                   (recur (rest n-stack) (map (fn [x] (merge default-contact {:point x :face1 (:side n) :face2 (map #(matrix/add % (-> n :side first)) start-points)}))
                                              [(:min-point side2) (:max-point side2)]))
                   (or (and (math/eps= (matrix/x (:normal n)) 0)
                            (math/eps> (matrix/y (:normal n)) 0))
                       (math/eps> (matrix/x (:normal n)) 0))
                   (cond
                    (math/eps= (:min side1) (:max side2))
                    (recur (rest n-stack) [(merge default-contact {:point (:min-point side1)})])
                    (math/eps= (:max side1) (:min side2))
                    (recur (rest n-stack) [(merge default-contact {:point (:max-point side1)})])
                    (and (math/eps= (:min side1) (:min side2)) (math/eps= (:max side2) (:max side1)))
                    (recur (rest n-stack) (map (fn [x] (merge default-contact {:point x :face1 (:side n) :face2 (map #(matrix/add % (-> n :side first)) start-points)}))
                                               [(:min-point side2) (:max-point side2)]))
                    (and (math/eps< (:min side1) (:min side2)) (math/eps< (:min side2) (:max side1)) (math/eps< (:max side1) (:max side2)))
                    (recur (rest n-stack) (map (fn [x] (merge default-contact {:point x :face1 (:side n) :face2 (map #(matrix/add % (-> n :side first)) start-points)}))
                                               [(:min-point side2) (:max-point side1)]))
                    (and (math/eps< (:min side2) (:min side1)) (math/eps< (:min side1) (:max side2)) (math/eps< (:max side2) (:max side1)))
                    (recur (rest n-stack) (map (fn [x] (merge default-contact {:point x :face1 (:side n) :face2 (map #(matrix/add % (-> n :side first)) start-points)}))
                                               [(:min-point side1) (:max-point side2)]))
                    :else
                    (recur (rest n-stack) accum))
                   :else
                   (recur (rest n-stack) accum)))))))
         accum))))
