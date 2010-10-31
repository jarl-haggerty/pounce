(ns pounce.body
  (:import java.awt.Color)
  (:use pounce.shape
        pounce.math
        pounce.contact
        pounce.render))
    
(defstruct body-struct
  :type
  :transformation 
  :shapes 
  :inertia-tensor 
  :linear-momentum 
  :angular-momentum
  :linear-velocity
  :angular-velocity 
  :mass
  :center
  :center-of-mass
  :radius)

(def default-body-map 
  (struct-map body 
    :type :body
    :transformation identity-transform
    :shapes []
    :inertia-tensor 0
    :linear-momentum (column 0 0)
    :angular-momentum 0
    :linear-velocity (column 0 0)
    :angular-velocity 0
    :mass 0
    :center-of-mass (column 0 0)
    :center (column 0 0)
    :radius 0))

(defn radius 
  ([center shapes]
     (loop [stack shapes result 0]
       (if (empty? stack)
         result
         (recur (rest stack) (max result (farside-distance center))))))
  ([body] (radius (:center body) (:shapes body))))

(defn body [trans & shapes] 
  (let [mass (if (== (count shapes) 1) (:mass (first shapes)) (reduce #(+ (:mass %1) (:mass %2)) shapes))
        center (if (== (count shapes) 1) (:center (first shapes)) (/ (reduce #(+ (:center %1) (:center %2)) shapes) (count shapes)))
        centerOfMass (if (or (is-infinite mass) (== (count shapes) 1))
                       center
                       (/ (reduce #(+ (* (:center %1) (:mass %1)) (* (:center %2) (:mass %2))) shapes) (count shapes)))]
    (struct body-struct
            :body
            trans
            shapes
            0
            (column 0 0)
            0
            (column 0 0)
            0
            mass
            center
            centerOfMass
            (radius center shapes))))

(defn integrate-derivatives [before linear-impulse angular-impulse]
  (let [new-linear-momentum (+ (:linear-momentum before) linear-impulse)
        new-angular-momentum (+ (:angular-momentum before) angular-impulse)]
    (struct body
            :body
            (:transformation before)
            (:shapes before)
            (:inertia-tensor before)
            new-linear-momentum
            new-angular-momentum
            (/ new-linear-momentum (:mass before))
            (/ new-angular-momentum (:mass before))
            (:mass before)
            (:center-of-mass before)
            (:center before)
            (:radius before))))

(defn integrate-transform [before delta]
  (struct body
          :body
          (let [center (* (:transform before) (:center-of-mass before))]
            (transform (+
                        (* (:rotation (:transform before))
                           (- (:translation (:transform before))
                              center))
                        center)
                       (+ (:rotation (:transformation before)) (rotation (* (:angular-velocity before) delta)))))
          (:shapes before)
          (:inertia-tensor before)
          (:linear-momentum before)
          (:angular-momentum before)
          (:linear-momentum before)
          (:angular-momentum before)
          (:mass before)
          (:center-of-mass before)
          (:center before)
          (:radius before)))

(defmethod render :body [body g]
           (doseq [shape (:shapes body)]
             (render shape transform g)))
(defn collision 
  ([shape1 body1 shape2 body2 delta]
     (let [speed (- (dot (unit (- (:center body2 (:center body1)))) (:linear-velocity body1))
                    (dot (unit (- (:center body2 (:center body1)))) (:linear-velocity body2)))
           velocity (* (unit (- (:center body2 (:center body1)))) speed)
           t1 (:transformation body1)]
           (loop [n-stack (normals shape1 (* (transpose (:rotation t1)) velocity)) accum []]
             (let [n (first n-stack)]
               (let [projection (project shape2 (first n))
                     points (nth projection 2)]
                 (if (> (first projection) (* (- speed) delta))
                   nil
                   (let [contact-time (if (= speed 0) 0 (/ (first projection) speed))]
                   (if (= (count points) 2)
                     (let [plane (unit (- (last n) (second n)))
                           side1 [0 (dot plane (- (last n) (second n)))]
                           side2 (sort-by first
                                          [(dot plane (- (first points) (second n))) (first points)]
                                          [(dot plane (- (second points) (second n))) (second points)])]
                       (cond
                        (= (first (first side1)) (first (second side2)))
                        (recur (rest n-stack) (conj accum (contact body1 body2 (first (first side1)) (first n) nil nil contact-time)))
                        (= (first (second side1)) (first (first side2)))
                        (recur (rest n-stack) (conj accum (contact body1 body2 (first (second side1)) (first n) nil nil contact-time)))
                        (and (< (first (first side1)) (first (second side2)))
                             (> (first (second side1)) (first (second side2))))
                        (if (< (first (first side1)) (first (first side2)))
                          (recur (rest n-stack) (concat accum
                                                        [(contact body1 body2 (second (first side2)) (first n)
                                                                   (- (last n) (second n))
                                                                   (- (second (second side2)) (second (first side2))) contact-time)
                                                         (contact body1 body2 (second (second side2)) (first n)
                                                                   (- (last n) (second n))
                                                                   (- (second (second side2)) (second (first side2))) contact-time)]))
                          (recur (rest n-stack) (concat accum
                                                        [(contact body1 body2 (second (first side1)) (first n)
                                                                   (- (last n) (second n))
                                                                   (- (second (second side2)) (second (first side2))) contact-time)
                                                         (contact body1 body2 (second (second side2)) (first n)
                                                                   (- (last n) (second n))
                                                                   (- (second (second side2)) (second (first side2))) contact-time)])))
                        :else (recur (rest n-stack) (concat accum
                                                            [(contact body1 body2 (second (first side2)) (first n)
                                                                      (- (last n) (second n))
                                                                      (- (second (second side2)) (second (first side2))) contact-time)
                                                             (contact body1 body2 (second (second side1)) (first n)
                                                                      (- (last n) (second n))
                                                                      (- (second (second side2)) (second (first side2))) contact-time)]))))
                     (recur (rest n-stack) (conj accum (contact body1 body2 (first points) (first n) nil nil contact-time)))))))))))
  ([body1 body2 delta]
    (let 
      [contacts 
       (apply interleave
              (for [shape1 (:shapes body1) shape2 (:shapes body2)]
                [(collision shape1 body1 shape2 body2 delta) (collision shape1 body1 shape2 body2 delta)]))]
       minTime (reduce min (map #(:time %) contacts))
       contact (filter #(<= minTime (:time-point %)) contacts)
      contact)))
  
  
