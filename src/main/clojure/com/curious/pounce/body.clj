(ns com.curious.pounce.body
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
                 :angular-momentum 0
                 :mass positive-infinity
                 :center-of-mass (matrix 0 0)
                 :center (matrix 0 0)
                 :radius 0}
       {:type :body}))


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
                (transform (+ (* (:rotation (:transform before)) rotation-matrix
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
           (doseq [shape (:shapes body)]
             (render (* (:transform body) shape) g)))

(defn collision 
  ([raw-shape1 body1 raw-shape2 body2 delta]
     (let [linear-velocity1 (/ (:linear-momentum body1) (:mass body1))
           linear-velocity2 (/ (:linear-momentum body2) (:mass body2))
           angular-velocity1 (/ (:angular-momentum body1) (:moment-of-inertia body1))
           angular-velocity2 (/ (:angular-momentum body2) (:moment-of-inertia body2))
           shape1 (transform-about (:transform body1) (:center-of-mass body1) raw-shape1)
           shape2 (transform-about (:transform body2) (:center-of-mass body2) raw-shape2)
           velocity (- linear-velocity2 linear-velocity1)]
       (loop [n-stack (normals shape1 (- velocity)) accum [{:time negative-infinity}]]
         (if-let [n (first n-stack)]
           (let [speed (dot (:normal n) velocity)
                 proj (projection (- shape2 (-> n :side first)) (:normal n))
                 start-points (:start-points proj)]
             ;(println start-points)
             (if (> (+ (:start proj) (* speed delta)) 0)
               nil
               (let [contact-time (if (= speed 0) 0 (/ (:start proj) (- speed)))
                     default-contact (with-meta {:body1 (:id body1) :body2 (:id body2) :normal (:normal n) :time contact-time} {:type :contact})]
                 (cond
                  (< contact-time (:time (first accum)))
                  (recur (rest n-stack) accum)
                  (= (count start-points) 1)
                  (recur (rest n-stack) [(merge default-contact {:point (first start-points)})])
                  :else
                  (let [plane (unit (- (second (:side n))
                                       (first (:side n))))
                        side1 {:min 0 :min-point (first (:side n))
                               :max (length (- (second (:side n)) (first (:side n)))) :max-point (second (:side n))}
                        side2 (let [first-dot (dot plane (first start-points))
                                    second-dot (dot plane (second start-points))
                                    temp (sort-by first
                                                  [[first-dot (+ (* first-dot plane) (first (:side n)))]
                                                   [second-dot (+ (* second-dot plane) (first (:side n)))]])]
                                {:min (first (first temp)) :min-point (second (first temp))
                                 :max (first (second temp)) :max-point (second (second temp))})]
                    ;(println "hello")
                    ;(println plane)
                    ;(println side1)
                    ;(println side2)
                    (cond
                     (= (:min side1) (:max side2))
                     (recur (rest n-stack) [(merge default-contact {:point (:min-point side1)})])
                     (= (:max side1) (:min side2))
                     (recur (rest n-stack) [(merge default-contact {:point (:max-point side1)})])
                     (and (< (:min side1) (:min side2)) (< (:max side2) (:max side1)))
                     (recur (rest n-stack) (map #(merge default-contact {:point % :face1 (:side n) :face2 (map + start-points (repeat 2 (-> n :side first)))})
                                                [(:min-point side2) (:max-point side2)]))
                     (and (< (:min side2) (:min side1)) (< (:max side1) (:max side2)))
                     (recur (rest n-stack) (map #(merge default-contact {:point % :face1 (:side n) :face2 (map + start-points (repeat 2 (-> n :side first)))})
                                                [(:min-point side1) (:max-point side1)]))
                     (and (< (:min side1) (:min side2)) (< (:min side2) (:max side1)))
                     (recur (rest n-stack) (map #(merge default-contact {:point % :face1 (:side n) :face2 (map + start-points (repeat 2 (-> n :side first)))})
                                                [(:min-point side2) (:max-point side1)]))
                     (and (< (:min side1) (:max side2)) (< (:max side2) (:max side1)))
                     (recur (rest n-stack) (map #(merge default-contact {:point % :face1 (:side n) :face2 (map + start-points (repeat 2 (-> n :side first)))})
                                                [(:min-point side1) (:max-point side2)]))
                     :else
                     (recur (rest n-stack) accum)))))))
           (let [temp (keep #(if (:point %)
                               (merge % {:point (transform-about (transform (* (:time %) linear-velocity1) (* (:time %) angular-velocity1))
                                                                 (:center-of-mass body1)
                                                                 (:point %))
                                         :normal (* (rotation (* (:time %)
                                                                 angular-velocity1))
                                                    (:normal %))
                                         :face1 (seq (apply transform-about
                                                            (transform (* (:time %) linear-velocity1) (* (:time %) angular-velocity1))
                                                            (:center-of-mass body1)
                                                            (:face1 %)))
                                         :face2 (seq (apply transform-about
                                                            (transform (* (:time %) linear-velocity2) (* (:time %) angular-velocity2))
                                                            (:center-of-mass body2)
                                                            (:face2 %)))}))
                            accum)]
             (when (= 11 (:id body1)) (println) (println) (println linear-velocity1))
             (doseq [x accum y x :when (= 11 (:id body1))] (println y))
             (when (= 11 (:id body1)) (println))
             (doseq [x temp y x :when (= 11 (:id body1))] (println y))
             temp
                 )))))
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
