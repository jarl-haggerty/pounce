(ns pounce.shape
  (:import java.awt.Color)
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:use pounce.math.math
        pounce.math.matrix
        pounce.render))

(defn polygon [raw-mass & raw-points]
  (let [[mass points] (if (number? raw-mass)
                        [raw-mass (map #(if (matrix? %) % (matrix %)) raw-points)]
                        [positive-infinity (map #(if (matrix? %) % (matrix %)) (cons raw-mass raw-points))])]
    (with-meta {:center (/ (reduce #(+ %1 %2) points) (count points))
                :mass mass
                :points points
                :normals (map #(normal (- %1 %2)) (conj (vec (rest points)) (first points)) points)}
      {:type :polygon})))

(defn circle
  ([center radius] (circle positive-infinity center radius))
  ([mass center radius]
     (with-meta  {:center (if (matrix? center) center (matrix center)) :mass mass :radius radius} {:type :circle})))

(defmulti normals (fn [shape & _] (:type (meta shape))))

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
             [{:normal direction-unit :side [side side]}]))

(defmulti projection (fn [shape plane] (:type (meta shape))))

(defmethod projection :polygon [shape plane]
           (let [points (circular-vector (:points shape))
                 front-point-index (apply min-key #(dot plane (points %)) (range (count (:points shape))))
                 front-point (points front-point-index)
                 front [(dot plane front-point)
                        (condp = (dot plane front-point)
                          (dot plane (points (inc front-point-index)))
                          [front-point (points (inc front-point-index))]
                          (dot plane (points (dec front-point-index)))
                          [(points (dec front-point-index)) front-point]
                          [front-point])]
                 back-point-index (apply max-key #(dot plane (points %)) (range (count (:points shape))))
                 back-point (points back-point-index)
                 back [(dot plane back-point)
                        (condp = (dot plane back-point)
                          (dot plane (points (inc back-point-index)))
                          [back-point (points (inc back-point-index))]
                          (dot plane (points (dec back-point-index)))
                          [(points (dec back-point-index)) back-point]
                          [back-point])]]
             {:start (first front) :stop (first back) :start-points (second front) :stop-points (second back)}))

(comment
  (defmethod projection :polygon [shape plane]
             (let [points (circular-vector (:points shape))
                   search (fn [condition]
                            (loop [accum 0 step (ceil (/ (count (:points shape)) 2)) from nil]
                              (if (= <= condition) (println (points accum) accum step from))
                              (condp condition (dot plane (points accum))
                              
                                (dot plane (points (+ accum step)))
                                (recur (+ accum step) (ceil (/ step 2)) accum)

                                (dot plane (points (- accum step)))
                                (recur (- accum step) (ceil (/ step 2)) accum)
                              
                                [(dot plane (points accum))
                                 (condp = (dot plane (points accum))
                                     (dot plane (points (inc accum)))
                                   [(points accum) (points (inc accum))]
                                   (dot plane (points (dec accum)))
                                   [(points (dec accum)) (points accum)]
                                   [(points accum)])])))
                   front (search <)
                   back (search >)]
               {:start (first front) :stop (first back) :start-points (second front) :stop-points (second back)})))

(defmethod projection :circle [shape plane]
           (let [plane-unit (unit plane)]
             {:start (- (dot (:center shape) plane) (:radius shape))
              :stop (+ (dot (:center shape) plane) (:radius shape))
              :start-points [(- (:center shape) (* (:radius shape) plane-unit))]
              :stop-points [(+ (:center shape) (* (:radius shape) plane-unit))]}))

(defmulti farside-distance (fn [shape origin] (:type (meta shape))))

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

(defmulti moment-of-inertia (fn [x & _]) (:type (meta x)))
(defmethod moment-of-inertia :polygon
  ([shape] (moment-of-inertia shape (:center shape)))
([shape axis]
   (for [triangle (map (vector (:center shape) %1 %2) (:points shape) (conj (vec (rest points)) (first points)))]
       (let [base (length (- (nth triangle 1) (nth triangle 0)))
             peak (dot (unit (- (nth triangle 1) (nth triangle 0)))
                       (unit (- (nth triangle 2) (nth triangle 0))))
             height (sqrt (- (length-squared (- (nth triangle 2) (nth triangle 0))) (pow peak 2)))
             center (/ (reduce + triangle) 3)]
         (+ (/ (+ (* (pow b 3) h) (* -1 (pow base 2) height peak) (* base height (pow peak 2)) (* base (pow height 3))) 36)
            (* (:mass shape) (length-squared (- center axis))))))))
(defmethod moment-of-inertia :circle
  ([shape] (moment-of-inertia shape (:center shape)))
  ([shape axis] (+ (/ (* (:mass shape) (pow (:radius shape) 2)) 2)
                   (* (:mass shape) (length-squared (- (:center shape) axis))))))
