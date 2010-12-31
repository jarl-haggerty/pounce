(ns com.curious.pounce.shape
  "Defines shape structure and functions for working with them."
  (:import java.awt.Color)
  (:use com.curious.pounce.math.math
        com.curious.pounce.math.matrix
        com.curious.pounce.render))

(defprotocol Shape
  (normals [this direction])
  (projection [this line])
  (farsideDistance [this point])
  (collision [this that])
  (getMass [this])
  (getCenter [this]))

(defprotocol PolygonProtocol
  (getPoints [this]))

(deftype Polygon [points mass center normals area moment-of-inertia]
  Object
  (equals [this that] (and (every? identity (map equals points (getPoints that)))
                           (eps= mass (getMass that))))
  Shape
  (normals [this direction] (filter #(<= 0 (dot direction (:normal %)))
                                    (map #(hash-map :normal %1 :side [%2 %3])
                                         normals
                                         points
                                         (conj (vec (rest points)) (first points)))))
  (projection [this line] (let [circ-points (circular-vector points)
                                 front-point-index (apply min-key #(dot line (circ-points %)) (range (count points)))
                                 front-point (circ-points front-point-index)
                                 front [(dot line front-point)
                                        (condp = (dot line front-point)
                                            (dot line (circ-points (inc front-point-index)))
                                          [front-point (circ-points (inc front-point-index))]
                                          (dot line (circ-points (dec front-point-index)))
                                          [(circ-points (dec front-point-index)) front-point]
                                          [front-point])]
                                 back-point-index (apply max-key #(dot line (circ-points %)) (range (count points)))
                                 back-point (circ-points back-point-index)
                                 back [(dot line back-point)
                                       (condp = (dot line back-point)
                                           (dot line (circ-points (inc back-point-index)))
                                         [back-point (circ-points (inc back-point-index))]
                                         (dot line (circ-points (dec back-point-index)))
                                         [(circ-points (dec back-point-index)) back-point]
                                         [back-point])]]
                             {:start (first front) :stop (first back) :start-points (second front) :stop-points (second back)}))
  (farsideDistance [this point] (apply max (map #(length (sub % point)) points)))
  (getMass [this] mass)
  (getCenter [this] center)
  (collision [this that] (collision-function this that))
  PolygonProtocol
  (getPoints [this] points)
  Transformable
  (transform [this trans] (apply polygon mass (map #(transform % trans) points)))
  (translate [this translation (apply polygon mass (map #(translate % translation) points))])
  (rotate [this rotation] (apply polygon mass (map #(rotate % rotation) points)))
  Renderable
  (render [this graphics]
          (.setColor g Color/white)
          (doseq [[v1 v2] (map vector
                               points
                               (conj (vec (rest points)) (first points)))]
            (.drawLine g (x v1) (- (-> g .getClipBounds .getHeight) (y v1))
                       (x v2) (- (-> g .getClipBounds .getHeight) (y v2))))))

(deftype Circle [center mass radius area moment-of-inertia]
  Object
  (equals [this that] (and (equals center (getCenter that))
                           (equals mass (getMass that))))
  Shape
  (normals [this direction] (let [direction-unit (unit direction)
                                 side (mul radius direction-unit)]
                             [{:normal direction-unit :side [side (add side (mul (normal direction-unit) (- eps)))]}]))
  (projection [this line] (let [line-unit (unit line)]
                             {:start (- (dot (:center shape) line) (:radius shape))
                              :stop (+ (dot (:center shape) line) (:radius shape))
                              :start-points [(- (:center shape) (* (:radius shape) line-unit))]
                              :stop-points [(+ (:center shape) (* (:radius shape) line-unit))]}))
  (farsideDistance [this point] (add (length (sub (:center shape) origin)) (:radius shape)))
  Transformable
  (transform [this trans] (circle mass (* center transform) radius))
  (translate [this translation] (circle mass (* center translation) radius))
  (rotate [this rotation] (circle mass (* center rotation) radius))
  (getMass [this] mass)
  (getCenter [this])
  (collision [this that] (collision-function this that))
  Renderable
  (render [this graphics]
          (.setColor g Color/white)
          (.drawOval g (- (x center) radius)
                     (- (y center) radius)
                     radius
                     radius)))

(defn polygon [raw-mass & raw-points]
  "Creates a shape with mass as the first argument and vetices as the rest, or, if the first argument isn't a scalar all the
   arguments will be used as vertices and the shape will have an infinite mass"
  (let [[mass points] (if (number? raw-mass)
                        [raw-mass (map mat raw-points)]
                        [positive-infinity (map mat (cons raw-mass raw-points))])
        center (div (reduce add points) (count points))
        sides (map #(vector %1 %2) points (conj (vec (rest points)) (first points)))
        area (/ (reduce + (map #(cross (sub (nth % 0) center) (sub (nth % 1) center)) sides)) 2)
        moment-of-inertia (* mass 1/6
                             (reduce /
                                     (reduce #(map + %1 %2)
                                             (for [pair (map #(vector (sub (first %) center) (sub (second %) center)) sides)]
                                               (let [a (cross (first pair) (second pair))
                                                     b (+ (dot (first pair) (first pair))
                                                          (dot (first pair) (second pair))
                                                          (dot (second pair) (second pair)))]
                                                 [(* a b) a])))))]
    (Polygon. points
              mass
              center
              (map #(normal (- (second %) (first %))) sides)
              area
              moment-of-inertia)))

(defn circle
  "Creates a circle with the specified mass or inifinite mass if not defined and with the
   specified center and radius."
  ([center radius] (circle positive-infinity center radius))
  ([mass center radius]
     (Circle. (mat center) mass radius (* pi (pow radius 2)) (* mass radius radius 1/2))))

(defn- collision-function
  "Calculates all the points of collisions that will occur in the next delta seconds between two bodies, neglecting rotation."
  ([shape1 shape2]
     (if-let [direction1 (penetration shape1 shape2 (- (getCenter shape2) (getCenter shape1)))]
       (if-let [direction2 (penetration shape2 shape1 (- (getCenter shape1) (getCenter shape2)))]
         (min-key #(:depth (first %)) direction1 direction2)
         nil)
       nil))
  ([shape1 shape2 direction]
     (loop [n-stack (normals shape1 direction) accum [{:depth positive-infinity :type 0}]]
       (if-let [n (first n-stack)]
         (let [proj (projection (- shape2 (-> n :side first)) (:normal n))
               start-points (:start-points proj)]
           (if (> (:start proj) 0)
             nil
             (let [depth (- (:start proj))
                   default-contact (with-meta {:normal (:normal n) :depth depth} {:type :contact})]
               (cond
                (< (:depth (first accum)) depth)
                (recur (rest n-stack) accum)
                (= (count start-points) 1)
                (let [plane (unit (- (second (:side n))
                                     (first (:side n))))
                      side {:min 0 :min-point (first (:side n))
                            :max (length (- (second (:side n)) (first (:side n)))) :max-point (second (:side n))}
                      point (dot plane (first start-points))]
                  (if (and (< (:min side) point) (< point (:max side)))
                    (recur (rest n-stack) [(merge default-contact {:point (+ (* point plane) (first (:side n))) :debug [side point start-points]})])
                    (recur (rest n-stack) accum)))
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
                  (cond
                   (and (< (:min side1) (:min side2)) (< (:max side2) (:max side1)))
                   (recur (rest n-stack) (map #(merge default-contact {:point % :face1 (:side n) :face2 (map + start-points (repeat 2 (-> n :side first))) :type 2})
                                              [(:min-point side2) (:max-point side2)]))
                   (or (and (zero? (x (:normal n))) (> (y (:normal n)) 0)) (> (x (:normal n)) 0))
                   (cond
                    (= (:min side1) (:max side2))
                    (recur (rest n-stack) [(merge default-contact {:point (:min-point side1) :type 3})])
                    (= (:max side1) (:min side2))
                    (recur (rest n-stack) [(merge default-contact {:point (:max-point side1) :type 4})])
                    (and (< (:min side1) (:min side2)) (< (:min side2) (:max side1)) (< (:max side1) (:max side2)))
                    (recur (rest n-stack) (map #(merge default-contact {:point % :face1 (:side n) :face2 (map + start-points (repeat 2 (-> n :side first))) :type 5})
                                               [(:min-point side2) (:max-point side1)]))
                    (and (< (:min side2) (:min side1)) (< (:min side1) (:max side2)) (< (:max side2) (:max side1)))
                    (recur (rest n-stack) (map #(merge default-contact {:point % :face1 (:side n) :face2 (map + start-points (repeat 2 (-> n :side first))) :type 6})
                                               [(:min-point side1) (:max-point side2)]))
                    :else
                    (recur (rest n-stack) accum))
                   :else
                   (recur (rest n-stack) accum)))))))
         accum))))
