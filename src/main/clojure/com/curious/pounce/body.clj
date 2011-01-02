(ns com.curious.pounce.body
  "Defines the body structure and methods for simulating forces on it and calculating collisions."
  (:import java.awt.Color)
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:require [com.curious.pounce.shape :as shape]
            [com.curious.pounce.math.math :as math]
            [com.curious.pounce.math.matrix :as matrix]
            [com.curious.pounce.render :as render]))

(defrecord Body [transformation shapes moment-of-inertia linear-momentum linear-velocity angular-momentum angular-velocity mass center-of-mass kinematic]
  render/Renderable
  (render [this graphics] (doseq [shape shapes]
                            (render (matrix/transform transform shape) graphics))))

(defn body [raw-trans & raw-shapes]
  "Creates a body from the specified transform and shapes."
  (let [[trans shapes] (if (instance? matrix/Transformation raw-trans)
                         [raw-trans raw-shapes]
                         [matrix/identity-transform (cons raw-trans raw-shapes)])
        mass (reduce + (map :mass shapes))
        center-of-mass (if (math/is-infinite mass)
                         (matrix/div (reduce matrix/add (map :center shapes)) (count shapes))
                         (matrix/div (reduce matrix/add (map #(* (:center %) (:mass %)) shapes)) (count shapes)))]
    (Body. trans
           shapes
           (if (math/is-infinite mass)
             math/positive-infinity
             (reduce +
                     (map #(+ (:moment-of-inertia %)
                              (* (:mass %) (matrix/length-squared (matrix/sub (:center %) center-of-mass))))
                          shapes)))
           (matrix/create 0 0)
           (matrix/create 0 0)
           0
           0
           mass
           center-of-mass
           false)))

(defn update [this delta]
  (let [rotation-matrix (matrix/rotation (* delta (:angular-velocity this)))]
    (assoc this
      :transform (let [center (matrix/transform (:center-of-mass this) (:transformation this))]
                   (matrix/transformation (matrix/add (:translation transform)
                                                      (matrix/mul linear-velocity delta)
                                                      (matrix/mul rotation-matrix
                                                                  (matrix/sub (:translation transform)
                                                                              center))
                                                      (:translation transform)
                                                      center)
                                          (matrix/mul (:rotation transform) rotation-matrix)))
      :linear-velocity (matrix/div (:linear-momentum this) (:mass this))
      :angular-velocity (/ (:angular-momentum this) (:moment-of-inertia this)))))

(defn collisions [this that]
  (collisions [this that] (map #(assoc %
                                  :body1 (:id this)
                                  :body2 (:id that))
                               (->> (for [shape1 (:shapes this)
                                          shape2 (:shapes that)]
                                      (shape/collision (shape/transform (:transformation that) shape1)
                                                       (shape/transform (:transformation that) shape2)))
                                    flatten
                                    (filter identity)))))
