(ns com.curious.pounce.shape
  "Defines shape structure and functions for working with them."
  (:import java.awt.Color)
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:use com.curious.pounce.math.math
        com.curious.pounce.math.matrix
        com.curious.pounce.render))

(defn polygon [raw-mass & raw-points]
  "Creates a shape with mass as the first argument and vetices as the rest, or, if the first argument isn't a scalar all the
   arguments will be used as vertices and the shape will have an infinite mass"
  (let [[mass points] (if (number? raw-mass)
                        [raw-mass (map mat raw-points)]
                        [positive-infinity (map mat (cons raw-mass raw-points))])
        center (/ (reduce + points) (count points))
        sides (map #(vector %1 %2) points (conj (vec (rest points)) (first points)))
        area (/ (reduce + (map #(cross (- (nth % 0) center) (- (nth % 1) center)) sides)) 2)
        moment-of-inertia (* mass 1/6
                             (reduce /
                                     (reduce #(map + %1 %2)
                                             (for [pair (map #(vector (- (first %) center) (- (second %) center)) sides)]
                                               (let [a (cross (first pair) (second pair))
                                                     b (+ (dot (first pair) (first pair))
                                                          (dot (first pair) (second pair))
                                                          (dot (second pair) (second pair)))]
                                                 [(* a b) a])))))]
    (with-meta {:center center
                :mass mass
                :points points
                :normals (map #(normal (- (second %) (first %))) sides)
                :area area
                :moment-of-inertia moment-of-inertia}
      {:type :polygon})))

(defn circle
  "Creates a circle with the specified mass or inifinite mass if not defined and with the
   specified center and radius."
  ([center radius] (circle positive-infinity center radius))
  ([mass center radius]
     (with-meta  {:center (if (matrix? center) center (matrix center))
                  :mass mass
                  :radius radius
                  :area (* pi (pow radius 2))
                  :moment-of-inertia (* mass radius radius 1/2)}
       {:type :circle})))

(defmulti normals
  "Calculates the normals of a shape"
  (fn [shape & _] (:type (meta shape))))

(defmethod normals :polygon
  ([shape] (map #(hash-map :normal %1 :side [%2 %3])
                (:normals shape)
                (:points shape)
                (conj (vec (rest (:points shape))) (first (:points shape)))))
  ([shape direction]
     (filter #(<= 0 (dot direction (:normal %))) (normals shape))))

(defmethod normals :circle [shape direction]
           (let [direction-unit (unit direction)
                 side (* (:radius shape) direction-unit)]
             [{:normal direction-unit :side [side (+ side (* (- eps) (normal direction-unit)))]}]))

(defmulti projection
  "Calculates the projection of the shape onto the requested line."
  (fn [shape line] (:type (meta shape))))

(defmethod projection :polygon [shape line]
           (let [points (circular-vector (:points shape))
                 front-point-index (apply min-key #(dot line (points %)) (range (count (:points shape))))
                 front-point (points front-point-index)
                 front [(dot line front-point)
                        (condp = (dot line front-point)
                          (dot line (points (inc front-point-index)))
                          [front-point (points (inc front-point-index))]
                          (dot line (points (dec front-point-index)))
                          [(points (dec front-point-index)) front-point]
                          [front-point])]
                 back-point-index (apply max-key #(dot line (points %)) (range (count (:points shape))))
                 back-point (points back-point-index)
                 back [(dot line back-point)
                        (condp = (dot line back-point)
                          (dot line (points (inc back-point-index)))
                          [back-point (points (inc back-point-index))]
                          (dot line (points (dec back-point-index)))
                          [(points (dec back-point-index)) back-point]
                          [back-point])]]
             {:start (first front) :stop (first back) :start-points (second front) :stop-points (second back)}))

(comment
  (defmethod projection :polygon [shape line]
             (let [points (circular-vector (:points shape))
                   search (fn [condition]
                            (loop [accum 0 step (ceil (/ (count (:points shape)) 2)) from nil]
                              (if (= <= condition) (println (points accum) accum step from))
                              (condp condition (dot plane (points accum))
                              
                                (dot line (points (+ accum step)))
                                (recur (+ accum step) (ceil (/ step 2)) accum)

                                (dot line (points (- accum step)))
                                (recur (- accum step) (ceil (/ step 2)) accum)
                              
                                [(dot line (points accum))
                                 (condp = (dot line (points accum))
                                     (dot line (points (inc accum)))
                                   [(points accum) (points (inc accum))]
                                   (dot line (points (dec accum)))
                                   [(points (dec accum)) (points accum)]
                                   [(points accum)])])))
                   front (search <)
                   back (search >)]
               {:start (first front) :stop (first back) :start-points (second front) :stop-points (second back)})))

(defmethod projection :circle [shape line]
           (let [line-unit (unit line)]
             {:start (- (dot (:center shape) line) (:radius shape))
              :stop (+ (dot (:center shape) line) (:radius shape))
              :start-points [(- (:center shape) (* (:radius shape) line-unit))]
              :stop-points [(+ (:center shape) (* (:radius shape) line-unit))]}))

(defmulti farside-distance
  "Calculates the distance from a point to the farside of a shape."
  (fn [shape origin] (:type (meta shape))))

(defmethod farside-distance :polygon [shape origin]
           (apply max (map #(length (- % origin)) (:points shape))))

(defmethod farside-distance :circle [shape origin]
           (+ (length (- (:center shape) origin)) (:radius shape)))

(defmethod render :polynomial [shape g]
           (.setColor g Color/white)
           (doseq [[v1 v2] (map vector
                                (conj (vec (rest (:points shape))) (first (:points shape))))]
             (.drawLine g (x v1) (y v1) (x v2) (y v2))))

(defmethod render :circle [shape g]
           (.setColor g Color/white)
           (.drawOval g (- (x (:center shape)) (:radius shape))
                      (- (y (:center shape)) (:radius shape))
                      (:radius shape)
                      (:radius shape)))

(defmethod equal [:polygon :polygon] [x y]
           (and (= (:center x) (:center y))
                (= (:mass x) (:mass y))
                (seq= (:points x) (:points y))
                (seq= (:normals x) (:normals y))))
(defmethod equal [:circle :circle] [x y]
           (and (= (:center x) (:center y))
                (= (:mass x (:mass y)))
                (= (:radius x) (:radius y))))
(defmethod equal [:circle :polygon] [x y] false)
(defmethod equal [:polygon :circle] [x y] false)


(defmethod multiply [:transform :polygon] [x y]
           (apply polygon (:mass y) (map #(* x %) (:points y))))

(defmethod multiply [:transform :circle] [x y]
           (circle (:mass y) (* x (:center y)) (:radius y)))

(defmethod add [:polygon :matrix] [x y] (apply polygon (:mass x) (map #(+ y %) (:points x))))
(defmethod add [:circle :matrix] [x y] (apply polygon (:mass x) (+ y (:center x)) (:radius x)))
(defmethod add [:matrix :polygon] [x y] (apply polygon (:mass y) (map #(+ x %) (:points y))))
(defmethod add [:matrix :circle] [x y] (apply polygon (:mass y) (+ x (:center y)) (:radius y)))
