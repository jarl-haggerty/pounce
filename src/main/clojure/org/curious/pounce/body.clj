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

(ns org.curious.pounce.body
  "Defines the body structure and methods for simulating forces on it and calculating collisions."
  (:import java.awt.Color)
  (:require [org.curious.pounce.shape :as shape]
            [org.curious.pounce.math.core :as math]
            [org.curious.pounce.math.matrix :as matrix]))

(defn body= [this that] (and (= (:transformation this) (:transformation that))
                             (= (:shapes this) (:shapes that))
                             (math/eps= (:moment-of-inertia this) (:moment-of-inertia that))
                             (= (:linear-momentum this) (:linear-momentum that))
                             (= (:linear-velocity this)  (:linear-velocity that))
                             (math/eps= (:angular-momentum this)  (:angular-momentum that))
                             (math/eps= (:angular-velocity this)  (:angular-velocity that))
                             (math/eps= (:mass this)  (:mass that))
                             (= (:center-of-mass this)  (:center-of-mass that))
                             (= (:kinematic this)  (:kinematic that))))

(defn create [raw-trans & raw-shapes]
  "Creates a body from the specified transform and shapes."
  (let [[trans shapes] (if (instance? org.curious.pounce.math.matrix.Transformation raw-trans)
                         [raw-trans raw-shapes]
                         [matrix/identity-transformation (cons raw-trans raw-shapes)])
        mass (reduce + (map :mass shapes))
        center-of-mass (if (math/is-infinite mass)
                         (matrix/div (reduce matrix/add (map :center shapes)) (count shapes))
                         (matrix/div (reduce matrix/add (map #(matrix/mul (:center %) (:mass %)) shapes)) (count shapes)))]
    {:transformation trans
     :shapes shapes
     :moment-of-inertia (if (math/is-infinite mass)
			  math/positive-infinity
			  (reduce +
				  (map #(+ (:moment-of-inertia %)
					   (* (:mass %) (matrix/length-squared (matrix/sub (:center %) center-of-mass))))
				       shapes)))
     :linear-momentum (matrix/column 0 0)
     :linear-velocity (matrix/column 0 0)
     :angular-momentum 0
     :angular-velocity 0
     :mass mass
     :center-of-mass center-of-mass
     :kinematic (math/is-infinite mass)}))

(defn update [this delta force torque velocity]
  (let [new-linear-momentum (matrix/add (matrix/mul velocity (:mass this)) (:linear-momentum this) (matrix/mul force delta))
        new-angular-momentum (+ (:angular-momentum this) (* torque delta))
        new-linear-velocity (if (:kinematic this)
                              (:linear-velocity this)
                              (matrix/div new-linear-momentum (:mass this)))
        new-angular-velocity (if (:kinematic this)
                              (:angular-velocity this)
                              (/ new-angular-momentum (:moment-of-inertia this)))
        rotation-matrix (matrix/rotation-matrix (* delta new-angular-velocity))
        center (matrix/transform (:center-of-mass this) (:transformation this))]
    (assoc this
      :transformation (matrix/transformation (matrix/add (matrix/mul rotation-matrix
                                                                     (matrix/sub (-> this :transformation :translation)
                                                                                 center))
							 (matrix/mul new-linear-velocity delta)
                                                         center)
                                             (matrix/mul (-> this :transformation :rotation) rotation-matrix))
      :linear-velocity new-linear-velocity
      :angular-velocity new-angular-velocity
      :linear-momentum new-linear-momentum
      :angular-momentum new-angular-momentum)))

(defn render [this graphics] (doseq [shape (:shapes this)]
			       (shape/render (shape/transform shape (:transformation this)) graphics)))
