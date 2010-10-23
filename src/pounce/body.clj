(ns pounce.body
  (:import (java.awt Color))
  (:use (pounce math shape contact render)))
    
(defstruct body 
  :type
  :transformation 
  :shapes 
  :inertiaTensor 
  :linearMomentum 
  :angularMomentum
  :linearVelocity
  :angularVelocity 
  :mass
  :center
  :centerOfMass
  :radius)

(def defaultBodyMap 
  (struct-map body 
    :type :body
    :transformation identity-transform
    :shapes []
    :inertiaTensor 0
    :linearMomentum (column 0 0)
    :angularMomentum 0
    :linearVelocity (column 0 0)
    :angularVelocity 0
    :mass 0
    :centerOfMass (column 0 0)
    :center (column 0 0)
    :radius 0))

(defn radius 
  ([center shapes]
    (loop [points (flatten (map :points shapes)) result positive-infinity]
      (if (empty? points)
        result
        (if (< (length (- (first points) center)) result)
          (recur (rest points) (length (- (first points) center)))
          (recur (rest points) result)))))
  ([body] (radius (:center body) (:shapes body))))

(defn create-body [trans & shapes] 
  (let [mass (if (== (count shapes) 1) (:mass (first shapes)) (reduce #(+ (:mass %1) (:mass %2)) shapes))
        center (if (== (count shapes) 1) (:center (first shapes)) (/ (reduce #(+ (:center %1) (:center %2)) shapes) (count shapes)))
        centerOfMass (if (or (isInfinite mass) (== (count shapes) 1))
                       center
                       (/ (reduce #(+ (* (:center %1) (:mass %1)) (* (:center %2) (:mass %2))) shapes) (count shapes)))]
    (struct body :body trans shapes 0 (column 0 0) 0 (column 0 0) 0 mass center centerOfMass (radius center shapes))))

(defn integrate-derivatives [before linear-impulse angular-impulse]
  ;(println "CLICK" linear-impulse)
  (let [new-linear-momentum (+ (:linearMomentum before) linear-impulse)
        new-angular-momentum (+ (:angularMomentum before) angular-impulse)]
    ;(println "CLICK" new-linear-momentum)
    (struct body
      :body
      (:transformation before)
      (:shapes before)
      (:inertiaTensor before)
      new-linear-momentum
      new-angular-momentum
      (/ new-linear-momentum (:mass before))
      (/ new-angular-momentum (:mass before))
      (:mass before)
      (:centerOfMass before)
      (:center before)
      (:radius before))))

(defn integrate-transform [before delta]
  (let [new-transformation (translate (:transformation before) (* (:linearVelocity before) delta))]
    (struct body
      :body
      (rotate 
        new-transformation
        (transform new-transformation (:centerOfMass before))
        (rotation-matrix (* (:angularVelocity before) delta)))
      (:shapes before)
      (:inertiaTensor before)
      (:linearMomentum before)
      (:angularMomentum before)
      (:linearMomentum before)
      (:angularMomentum before)
      (:mass before)
      (:centerOfMass before)
      (:center before)
      (:radius before))))

(defmethod render :body [body g]
  (loop [shapes (:shapes body)]
    (when-not (empty? shapes)
      (let [shape (map #(transform (:transformation body) %) (-> shapes first :points))]
        (loop [points (rest shape) lastPoint (first shape)]
          (when-not (empty? points)
            (.setColor g Color/white)
            (.drawLine g 
              (:x lastPoint)
              (- (-> g .getClipBounds .getHeight) (:y lastPoint))
              (:x (first points))
              (- (-> g .getClipBounds .getHeight) (:y (first points))))
            (recur (rest points) (first points))))   
        (.drawLine g 
          (-> shape last :x)
          (- (-> g .getClipBounds .getHeight) (-> shape last :y))
          (-> shape first :x)
          (- (-> g .getClipBounds .getHeight) (-> shape first :y)))
        (let [center (transform (:transformation body) (:center body))]
          (.drawOval g 
            (- (:x center) (:radius body))
            (- (-> g .getClipBounds .getHeight) (:y center) (:radius body))
            (* 2 (:radius body))
            (* 2 (:radius body))))
        (recur (rest shapes))))))

(defn collision 
  ([shape1 body1 shape2 body2 delta forwardCalculation]
    ;(println velocity)
    (let [velocity (- (:linearVelocity body2) (:linearVelocity body1))
          contact
          (loop
            [t1 (:transformation body1)
             t2 (:transformation body2)
             p1 (map #(transform t1 %) (:points shape1))
             p2 (map #(transform t2 %) (:points shape2))
             n1 (map #(* (:rotation t1) %) (:normals shape1))
             contact (create-contact :None negative-infinity)]
            ;(println "BOKABOKA" n1)
            (if (empty? n1)
              (do
                ;(println "RETURN" forwardCalculation)
                contact)
              (let [speed (* (first n1) velocity)]
                ;(println speed)
                ;(println "GOING")
                (if (and (> (* (first n1) (- (transform t1 (:center body1)) (transform t2 (:center body2)))) 0) (>= speed 0))
                  (recur t1 t2 (rest p1) p2 (rest n1) contact)
                  (let [[minimum closestPoints]
                        (loop [remaining p2 closestPoints () minimum positiveInfinity]
                          (if (empty? remaining)
                            [minimum closestPoints]
                            (let [temp (* (first n1) (- (first remaining) (first p1)))]
                              (cond
                                (= temp minimum) (recur (rest remaining) (-> remaining first (cons closestPoints)) minimum)
                                (< temp minimum) (recur (rest remaining) (list (first remaining)) temp)
                                (> temp minimum) (recur (rest remaining) closestPoints minimum)))))]
                    ;(println minimum closestPoints)
                    (if (> minimum (* (- speed) delta))
                      (do
                        ;(println "None")
                        (create-contact :None positive-infinity))
                      (let [timePoint (if (= speed 0) 0 (/ minimum (- speed)))]
                        ;(println "Not None")
                        ;(println minimum speed)
                        (if (<= timePoint (:time-point contact))
                          (recur t1 t2 (rest p1) p2 (rest n1) contact)
                          (let [p1Moved (map #(-> body1 :linearVelocity (* timePoint) (+ %)) p1)
                                p2Moved (map #(-> body2 :linearVelocity (* timePoint) (+ %)) p2)
                                closestPointsMoved (map #(-> body2 :linearVelocity (* timePoint) (+ %)) closestPoints)]
                            ;(println "BLIP" closestPointsMoved (-> closestPointsMoved count (= 2)))
                            ;(println delta)
                            ;(println timePoint)
                            (if (-> closestPointsMoved count (= 2))
                              (if (contains? closestPointsMoved (first p1Moved))
                                (do
                                  ;(println "VertexVertex")
                                  (recur t1 t2 (rest p1) p2 (rest n1) (create-contact :VertexVertex timePoint (first p1Moved))))
                                (let [nextPoint (if (empty? (rest p1Moved)) 
                                                  (->> shape1 :points first (transform (:transformation body1)))
                                                  (second p1Moved))]
                                  ;(println "The end" (X (- nextPoint (first p1Moved)) (- (second closestPointsMoved) (first closestPointsMoved))))
                                  (if (== (X (- nextPoint (first p1Moved)) (- (second closestPointsMoved) (first closestPointsMoved))) 0)
                                    (let [points (set (cons (first p1Moved) (cons nextPoint closestPointsMoved)))
                                          distance-map 
                                            (reduce merge
                                              (for [p1 points
                                                    p2 (disj points p1)]
                                                {(length-squared (- p1 p2)) [p1 p2]}))
                                          minimum (reduce min (keys distance-map))
                                          [point1 point2] (minimum distance-map)]
                                      ;(println "EdgeEdge")
                                      (recur t1 t2 (rest p1) p2 (rest n1) (create-contact :EdgeEdge timePoint point1 point2)))
                                    (do
                                      ;(println "EdgeVertex")
                                      (recur t1 t2 (rest p1) p2 (rest n1) (create-contact :EdgeVertex timePoint (first p1Moved)))))))
                              (if (contains? closestPointsMoved (first p1Moved))
                                (do
                                  ;(println "VertexVertex")
                                  (recur t1 t2 (rest p1) p2 (rest n1) (create-contact :VertexVertex timePoint (first closestPointsMoved))))
                                (do
                                  ;(println "EdgeVertex" timePoint closestPointsMoved contact)
                                  (recur t1 t2 (rest p1) p2 (rest n1) (create-contact :EdgeVertex timePoint (first closestPointsMoved)))))))))))))))
          ;temp (println "getting reverse contact" forwardCalculation)
          ;temp (println contact)
          reverseContact (if forwardCalculation (collision shape2 body2 shape1 body1 delta false) (create-contact :None negative-infinity))
          
          ];temp2 (println "GAGA" contact reverseContact)]
      ;(println "click" contact reverseContact)
      ;(println "final" contact reverseContact (> (:time-point contact) (:time-point reverseContact)) forwardCalculation)
      ;(println contact reverseContact)
      (if (> (:time-point contact) (:time-point reverseContact)) contact reverseContact)))
  ([shape1 body1 shape2 body2 delta] (collision shape1 body1 shape2 body2 delta true))
  ([body1 body2 delta]
    (let 
      [contacts 
       (seq 
         (for [shape1 (:shapes body1) shape2 (:shapes body2)] 
	       (let [temp (collision shape1 body1 shape2 body2 delta)]
	         ;(println "YAKA" temp)
	         temp)))
       ;temp (println "contacts" contacts)
       minTime (reduce min (map #(:time-point %) contacts))
       ;temp2 (println "minTime" minTime)
       contact (filter #(<= minTime (:time-point %)) contacts)]
      ;(println "Hello")
      ;(println "contact" contact)
      ;(println "minTime" minTime)
      (println (count contact))
      contact)))
  
  
