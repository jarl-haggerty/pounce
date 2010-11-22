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
  ([before delta]
     (let [rotation-matrix (rotation (* delta (/ (:angular-momentum before) (:moment-of-inertia before))))]
       (assoc before :transform
              (let [center (* (:transform before) (:center-of-mass before))]
                (transform (+
                            (* (:rotation (:transform before)) rotation-matrix
                               (- (:translation (:transform before))
                                  center))
                            center
                            (* delta (/ (:linear-momentum before) (:mass before))))
                           (* (:rotation (:transform before)) rotation-matrix))))))
  ([before linear-impulse angular-impulse delta]
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
       (persistent! after))))

(defmethod equal [:body :body] [x y]
  (= (:transform x) (:transform y))
  (seq= (:shapes x) (:shapes y))
  (= (:moment-of-inertia x) (:moment-of-inertia y))
  (= (:linear-momentum x) (:linear-momentum y))
  (= (:angular-momentum x)  (:angular-momentum y))
  (= (:mass x)  (:mass y))
  (= (:center-of-mass x)  (:center-of-mass y))
  (= (:center x)  (:center y))
  (= (:radius x)  (:radius y)))

(defmethod render :body [body g]
           (doseq [shape (:shapes body)]
             (render (* (:transform body) shape) g)))

(defn collision 
  ([raw-shape1 body1 raw-shape2 body2 delta]
     (let [shape1 (* (:transform body1) raw-shape1)
           shape2 (* (:transform body2) raw-shape2)
           velocity (- (/ (:linear-momentum body2) (:mass body2)) (/ (:linear-momentum body1) (:mass body1)))]
       (loop [n-stack (normals shape1 (- velocity)) accum [{:time negative-infinity}]]
         (if-let [n (first n-stack)]
           (let [speed (dot (:normal n) velocity)
                 proj (projection (- shape2 (-> n :side first)) (:normal n))
                 start-points (:start-points proj)]
             (if (> (+ (:start proj) (* speed delta)) 0)
               nil
               (let [contact-time (if (= speed 0) 0 (/ (:start proj) (- speed)))
                     default-contact {:body1 body1 :body2 body2 :normal (:normal n) :time contact-time}]
                 (cond
                  (< contact-time (:time (first accum)))
                  (recur (rest n-stack) accum)
                  (= (count start-points) 1)
                  (recur (rest n-stack) (merge default-contact {:point (first start-points) :face1 nil :face2 nil}))
                  :else
                  (let [plane (unit (- (second (:side n))
                                       (first (:side n))))
                        side1 {:min 0 :min-point (first (:side n))
                               :max (length (- (second (:side n)) (first (:side n)))) :max-point (second (:side n))}
                        side2 (let [temp (sort-by first
                                                  [[(dot plane (- (first start-points) (first (:side n)))) (first start-points)]
                                                   [(dot plane (- (second start-points) (first (:side n)))) (second start-points)]])]
                                {:min (first (first temp)) :min-point (second (first temp))
                                 :max (first (second temp)) :max-point (second (second temp))})]
                    (cond
                     (= (:min side1) (:max side2))
                     (recur (rest n-stack) [(merge default-contact {:point (:min-point side1) :face1 nil :face2 nil})])
                     (= (:max side1) (:min side2))
                     (recur (rest n-stack) [(merge default-contact {:point (:max-point side1) :face1 nil :face2 nil})])
                     (and (< (:min side1) (:min side2)) (< (:min side2) (:max side1)))
                     (recur (rest n-stack) (map #(merge default-contact {:point % :face1 (:side n) :face2 (map + start-points (repeat 2 (-> n :side first)))})
                                                [(:min-point side2) (:max-point side1)]))
                     (and (< (:min side1) (:max side2)) (< (:max side2) (:max side1)))
                     (recur (rest n-stack) (map #(merge default-contact {:point % :face1 (:side n) :face2 (map + start-points (repeat 2 (-> n :side first)))})
                                                [(:min-point side1) (:max-point side2)]))
                     (and (< (:min side1) (:min side2)) (< (:max side2) (:max side1)))
                     (recur (rest n-stack) (map #(merge default-contact {:point % :face1 (:side n) :face2 (map + start-points (repeat 2 (-> n :side first)))})
                                                [(:min-point side2) (:max-point side2)]))
                     (and (< (:min side2) (:min side1)) (< (:max side1) (:max side2)))
                     (recur (rest n-stack) (map #(merge default-contact {:point % :face1 (:side n) :face2 (map + start-points (repeat 2 (-> n :side first)))})
                                                [(:min-point side1) (:max-point side1)]))
                     :else
                     (recur (rest n-stack) accum)))))))
           (keep #(if-let [point (:point %)]
                    (assoc % :point (+ point
                                       (* (:time %)
                                          (/ (:linear-momentum body1)
                                             (:mass body1)))))
                    nil)
                accum)))))
  ([body1 body2 delta]
     (let 
         [contacts 
          (apply concat
                 (for [shape1 (:shapes body1)
                       shape2 (:shapes body2)]
                   (concat (collision shape1 body1 shape2 body2 delta)
                           (collision shape2 body2 shape1 body1 delta))))
          ;_ (doseq [contact contacts]
          ;    (println (-> contact
          ;                 (dissoc :body1)
         ;                  (dissoc :body2))))
          min-time (apply min positive-infinity (map #(:time %) contacts))]
       (filter #(<= min-time (:time %)) contacts))))
