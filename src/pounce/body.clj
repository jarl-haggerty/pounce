(ns pounce.body
  (:import java.awt.Color)
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:use pounce.shape
        pounce.math.math
        pounce.math.matrix
        pounce.render))
    
(def default-body-map
     (with-meta
       {:transform identity-transform
        :shapes []
        :moment-of-inertia positive-infinity
        :linear-momentum (matrix 0 0)
        :angular-momentum 0
        :linear-velocity (matrix 0 0)
        :angular-velocity 0
        :mass positive-infinity
        :center-of-mass (matrix 0 0)
        :center (matrix 0 0)
        :radius 0} {:type :body}))


(defn radius 
  ([center shapes]
     (apply max (map #(farside-distance % center) shapes)))
  ([body] (radius (:center body) (:shapes body))))

(defn body [raw-trans & raw-shapes] 
  (let [[trans shapes] (if (transform? raw-trans) [raw-trans raw-shapes] [identity-transform (cons raw-trans raw-shapes)])
        mass (reduce #(+ %1 (:mass %2)) (:mass (first shapes)) (rest shapes))
        center (/ (reduce #(+ %1 (:center %2)) (:center (first shapes)) (rest shapes)) (count shapes))
        center-of-mass (if (is-infinite mass)
                         center
                         (/ (reduce #(+ %1 (* (:center %2) (:mass %2))) (* (:center (first shapes)) (:mass (first shapes))) (rest shapes)) mass))]
    (merge default-body-map {:shapes shapes
                             :transform trans
                             :moment-of-inertia (reduce + (map #(moment-of-inertia % center-of-mass) shapes))
                             :mass mass
                             :center center
                             :center-of-mass center-of-mass
                             
                             :radius (radius center shapes)})))

(defn step [before linear-impulse angular-impulse delta]
  (let [new-linear-momentum (+ (:linear-momentum before) linear-impulse)
        new-angular-momentum (+ (:angular-momentum before) angular-impulse)
        rotation-matrix (rotation (* delta (/ new-angular-momentum (:moment-of-inertia before))))
        after (transient before)]
    (assoc! after :linear-momentum new-linear-momentum)
    (assoc! after :angular-momentum new-angular-momentum)
    (assoc! after :transform
            (let [center (* (:transform before) (:center-of-mass before))]
              (transform (+
                          (* (:rotation (:transform before)) rotation-matrix
                             (- (:translation (:transform before))
                                center))
                          center
                          (* delta (/ new-linear-momentum (:mass before))))
                         (* (:rotation (:transform before)) rotation-matrix))))
    (persistent! after)))

(defmethod render :body [body g]
           (doseq [shape (:shapes body)]
             (render (* (:transform body) shape) g)))

(defn collision 
  ([shape1 body1 shape2 body2 delta]
     (let [speed (- (dot (unit (- (:center body2) (:center body1))) (:linear-velocity body1))
                    (dot (unit (- (:center body2) (:center body1))) (:linear-velocity body2)))
           velocity (* (unit (- (:center body2) (:center body1))) speed)
           t1 (:transform body1)]
       (loop [n-stack (normals shape1 velocity) accum []]
         (if-let [n (first n-stack)]
           (let [proj (projection (- shape2 (-> n :side first)) (:normal n))
                 start-points (:start-points proj)]
             (if (> (:start proj) (* (- speed) delta))
               nil
               (let [contact-time (if (= speed 0) 0 (/ (first proj) speed))
                     default-contact {:body1 body1 :body2 body2 :normal (first n) :time contact-time}]
                 (if (= (count start-points) 2)
                   (let [plane (unit (apply - (:side n)))
                         side1 [0 (first (:side n)) (dot plane (apply - (:side n))) (second (:side n))]
                         side2 (sort-by first
                                        [(dot plane (- (first start-points) (second n))) (first start-points)]
                                        [(dot plane (- (second start-points) (second n))) (second start-points)])]
                     (cond
                      (= (first (first side1)) (first (second side2)))
                      (recur (rest n-stack) (conj accum (merge default-contact
                                                               {:point (second (first side1)) :face1 nil :face2 nil})))
                      (= (first (second side1)) (first (first side2)))
                      (recur (rest n-stack) (conj accum (merge default-contact
                                                               {:point (second (second side1)) :face1 nil :face2 nil})))
                      (and (< (first (first side1)) (first (second side2))) (> (first (second side1)) (first (second side2))))
                      (if (< (first (first side1)) (first (first side2)))           
                        (recur (rest n-stack) (concat accum
                                                      (map #(merge default-contact
                                                                   {:point % :face1 (:side n) :face2 start-points})
                                                           [(second (first side2)) (second (second side2))])))                                       
                        (recur (rest n-stack) (concat accum
                                                      (map #(merge default-contact
                                                                   {:point % :face1 (:side n) :face2 start-points})
                                                           [(second (first side1)) (second (second side2))]))))
                      :else
                      (recur (rest n-stack) (concat accum
                                                    (map #(merge default-contact
                                                                 {:point % :face1 (:side n) :face2 start-points})
                                                         [(second (first side2)) (second (second side1))])))))
                   (recur (rest n-stack) (conj accum (merge default-contact {:point (first start-points) :face1 nil :face2 nil})))))))
           accum))))
  ([body1 body2 delta]
     (let 
         [contacts 
          (apply interleave
                 (for [shape1 (map #(* (:transform body1) %) (:shapes body1))
                       shape2 (map #(* (:transform body2) %) (:shapes body2))]
                   [(collision shape1 body1 shape2 body2 delta)
                    (collision shape2 body2 shape1 body1 delta)]))
          min-time (reduce min (map #(:time %) contacts))]
       (filter #(<= min-time (:time-point %)) contacts))))
