(ns com.curious.pounce.body
  "Defines the body structure and methods for simulating forces on it and calculating collisions."
  (:import java.awt.Color)
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:use com.curious.pounce.shape
        com.curious.pounce.math.math
        com.curious.pounce.math.matrix
        com.curious.pounce.render))
    
(def default-body-map
     (with-meta {:transform identity-transform
                 :shapes []
                 :moment-of-inertia positive-infinity
                 :linear-momentum (matrix 0 0)
                 :linear-velocity (matrix 0 0)
                 :angular-momentum 0
                 :angular-velocity 0
                 :mass positive-infinity
                 :center-of-mass (matrix 0 0)
                 :center (matrix 0 0)
                 :radius 0
                 :kinematic false}
       {:type :body}))

(defn radius
  "Used to calculate the radius of an enclosing circle about the center of mass of a body."
  ([center shapes]
     (apply max (map #(farside-distance % center) shapes)))
  ([body] (radius (:center body) (:shapes body))))

(defn body [raw-trans & raw-shapes]
  "Creates a body from the specified transform and shapes."
  (let [[trans shapes] (if (transform? raw-trans) [raw-trans raw-shapes] [identity-transform (cons raw-trans raw-shapes)])
        mass (reduce #(+ %1 (:mass %2)) (:mass (first shapes)) (rest shapes))
        center (/ (reduce #(+ %1 (:center %2)) (:center (first shapes)) (rest shapes)) (count shapes))
        center-of-mass (if (is-infinite mass)
                         center
                         (/ (reduce #(+ %1 (* (:center %2) (:mass %2))) (* (:center (first shapes)) (:mass (first shapes))) (rest shapes)) mass))]
    (merge default-body-map {:shapes shapes
                             :transform trans
                             :moment-of-inertia (reduce +
                                                        (map #(+ (:moment-of-inertia %)
                                                                       (if (is-infinite (:mass %))
                                                                         positive-infinity
                                                                         (* (:mass %) (length-squared (- (:center %) center-of-mass)))))
                                                                   shapes))
                             :mass mass
                             :center center
                             :center-of-mass center-of-mass
                             :radius (radius center shapes)})))

(defn step
  "Simulates a step through a time of delta seconds on a body."
  ([before delta]
     (let [rotation-matrix (rotation (* delta (/ (:angular-momentum before) (:moment-of-inertia before))))]
       (assoc before :transform
              (let [center (* (:transform before) (:center-of-mass before))]
                (transform (+ (:translation (:transform before))
                              (* delta (/ (:linear-momentum before) (:mass before)))
                              (- (* rotation-matrix
                                    (- (:translation (:transform before))
                                       center))
                                 (- (:translation (:transform before))
                                    center)))
                           (* (:rotation (:transform before)) rotation-matrix))))))
  ([before linear-impulse angular-impulse delta]
     (let [new-linear-momentum (+ (:linear-momentum before) linear-impulse)
           new-angular-momentum (+ (:angular-momentum before) angular-impulse)
           rotation-matrix (rotation (* delta (/ new-angular-momentum (:moment-of-inertia before))))
           after (transient before)]
       (merge before {:linear-momentum new-linear-momentum
                      :angular-momentum new-angular-momentum
                      :transform (let [center (* (:transform before) (:center-of-mass before))]
                                   (transform (+ (* (:rotation (:transform before)) rotation-matrix
                                                    (- (:translation (:transform before))
                                                       center))
                                                 center
                                                 (* delta (/ new-linear-momentum (:mass before))))
                                              (* (:rotation (:transform before)) rotation-matrix)))}))))

(defmethod equal [:body :body] [x y]
           (and (= (:transform x) (:transform y))
                (seq= (:shapes x) (:shapes y))
                (= (:moment-of-inertia x) (:moment-of-inertia y))
                (= (:linear-momentum x) (:linear-momentum y))
                (= (:angular-momentum x)  (:angular-momentum y))
                (= (:mass x)  (:mass y))
                (= (:center-of-mass x)  (:center-of-mass y))
                (= (:center x)  (:center y))
                (= (:radius x)  (:radius y))))

(defmethod equal [:contact :contact] [x y]
           (and (= (:body1 x) (:body1 y))
                (= (:body2 x) (:body2 y))
                (= (:normal x) (:normal y))
                (= (:time x) (:time y))
                (= (:point x) (:point y))
                (seq= (:face1 x) (:face1 y))
                (seq= (:face2 x) (:face2 y))))

(defmethod render :body [body g]
           "Renders the shapes of a body"
           (doseq [shape (:shapes body)]
             (render (* (:transform body) shape) g)))

(defn get-collisions [body1 body2]
  (map #(assoc %
          :body1 (:id body1)
          :body2 (:id body2))
       (->> (for [shape1 (:shapes body1)
                 shape2 (:shapes body2)]
             (penetration (* (:transform body1) shape1) (* (:transform body2) shape2)))
           flatten
           (filter identity))))
