(ns com.curious.pounce.body
  "Defines the body structure and methods for simulating forces on it and calculating collisions."
  (:import java.awt.Color)
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:require [com.curious.pounce.shape :as shape]
            [com.curious.pounce.math.math :as math]
            [com.curious.pounce.math.matrix :as matrix]
            [com.curious.pounce.render :as render]))

(defprotocol BodyProtocol
  (update [this delta])
  (collisions [this that]))

(defrecord Body [id transform shapes moment-of-inertia linear-momentum angular-momentum mass center-of-mass kinematic]
  BodyProtocol
  (update [this delta] (let [rotation-matrix (matrix/rotation (* delta (/ angular-momentum moment-of-inertia)))]
                         (assoc this :transform
                                (let [center (transform center-of-mass transform)]
                                  (transformation (matrix/add (:translation transform)
                                                              (matrix/mul linear-momentum delta (/ mass))
                                                              (matrix/add (matrix/mul rotation-matrix
                                                                                      (matrix/sub (:translation transform)
                                                                                                  center))
                                                                          (:translation transform)
                                                                          center))
                                                  (matrix/mul (:rotation transform) rotation-matrix))))))
  (collisions [this that] (map #(assoc %
                                  :body1 id
                                  :body2 (:id that))
                               (->> (for [shape1 shapes
                                          shape2 (:shapes that)]
                                      (penetration (* transform shape1) (* (:transform that) shape2)))
                                    flatten
                                    (filter identity))))
  render/Renderable
  (render [this graphics] (doseq [shape shapes]
                            (render (matrix/transform transform shape) graphics))))

(defn body [raw-trans & raw-shapes]
  "Creates a body from the specified transform and shapes."
  (let [[trans shapes] (if (extends? matrix/TransformationProtocol raw-trans)
                         [raw-trans raw-shapes]
                         [matrix/identity-transform (cons raw-trans raw-shapes)])
        mass (reduce #(+ %1 (getMass %2)) (getMass (first shapes)) (rest shapes))
        center-of-mass (if (math/is-infinite mass)
                         (matrix/div (reduce #(matrix/add %1 (getCenter %2)) (getCenter (first shapes)) (rest shapes)) (count shapes))
                         (matrix/div (reduce #(matrix/add %1 (* (:center %2) (:mass %2))) (* (:center (first shapes)) (:mass (first shapes))) (rest shapes)) mass))]
    (Body. [trans
            shapes
            (reduce +
                    (map #(+ (:moment-of-inertia %)
                             (if (is-infinite (:mass %))
                               positive-infinity
                               (* (:mass %) (length-squared (matrix/sub (:center %) center-of-mass)))))
                         shapes))
            0
            0
            mass
            center-of-mass
            false])))
