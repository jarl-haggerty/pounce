(ns org.curious.pounce.body
  "Defines the body structure and methods for simulating forces on it and calculating collisions."
  (:import java.awt.Color)
  (:require [org.curious.pounce.shape :as shape]
            [org.curious.pounce.math.core :as math]
            [org.curious.pounce.math.matrix :as matrix]
            [org.curious.pounce.render :as render]))

(defrecord Body [transformation shapes moment-of-inertia linear-momentum linear-velocity angular-momentum angular-velocity mass center-of-mass kinematic]
  render/Renderable
  (render [this graphics] (doseq [shape shapes]
                            (render/render (shape/transform shape transformation) graphics))))

(defn create [raw-trans & raw-shapes]
  "Creates a body from the specified transform and shapes."
  (let [[trans shapes] (if (instance? org.curious.pounce.math.matrix.Transformation raw-trans)
                         [raw-trans raw-shapes]
                         [matrix/identity-transform (cons raw-trans raw-shapes)])
        mass (reduce + (map :mass shapes))
        center-of-mass (if (math/is-infinite mass)
                         (matrix/div (reduce matrix/add (map :center shapes)) (count shapes))
                         (matrix/div (reduce matrix/add (map #(matrix/mul (:center %) (:mass %)) shapes)) (count shapes)))]
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
  (let [new-linear-velocity (if (:kinematic this)
                              (:linear-velocity this)
                              (matrix/div (:linear-momentum this) (:mass this)))
        new-angular-velocity (if (:kinematic this)
                              (:angular-velocity this)
                              (/ (:angular-momentum this) (:moment-of-inertia this)))
        rotation-matrix (matrix/rotation-matrix (* delta new-angular-velocity))
        center (matrix/transform (:center-of-mass this) (:transformation this))]
    (assoc this
      :transformation (matrix/transformation (matrix/add (-> this :transformation :translation)
                                                         (matrix/mul new-linear-velocity delta)
                                                         (matrix/mul rotation-matrix
                                                                     (matrix/sub (-> this :transformation :translation)
                                                                                 center))
                                                         (-> this :transformation :translation)
                                                         center)
                                             (matrix/mul (-> this :transformation :rotation) rotation-matrix))
      :linear-velocity new-linear-velocity
      :angular-velocity new-angular-velocity)))

(defn collisions [this that]
  (comment (doseq [shape1 (:shapes this)
                   shape2 (:shapes that)]
             (println (shape/transform shape1 (:transformation this)))
             (println (shape/transform shape2 (:transformation that)))))
  (map #(assoc %
          :body1 (:id this)
          :body2 (:id that))
       (->> (for [shape1 (:shapes this)
                  shape2 (:shapes that)]
              (shape/collision (shape/transform shape1 (:transformation this))
                               (shape/transform shape2 (:transformation that))))
            flatten
            (filter identity))))

(defmulti process-contact #(hash-set (:body1 %) (:body2 %)))
