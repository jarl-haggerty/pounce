(ns pounce.shape
  (:import java.awt.Color)
  (:use pounce.math.matrix
        pounce.render))

(defstruct polygon-struct :center :mass :points :normals)
(defstruct circle-struct :center :mass :radius)

(defn polygon [mass & points]
  (let 
    [normals
     (loop [toProcess points lastPoint nil normals []]
       (if (empty? toProcess)
         (conj normals (normal (- (first points) (last points))))
         (if (nil? lastPoint)
           (recur (rest toProcess) (first toProcess) normals)
             (recur (rest toProcess) (first toProcess) (conj normals (normal (- (first toProcess) lastPoint)))))))]
    (with-meta (struct polygon-struct (/ (reduce #(+ %1 %2) points) (count points)) mass points normals) {:type :polygon})))
(println 1)
(defn circle [mass center radius]
  (with-meta (struct circle-struct center mass radius) {:type :circle}))

(defstruct normal-struct :normal :side)
(defmulti normals #(:type (meta %)))
(defmethod normals :polygon [shape & args]
           (let [raw-normals (map #(struct normal-struct %1 (- %3 %2))
                                  (:normals shape)
                                  (:points shape)
                                  (conj (rest (:points shape)) (first (:points shape))))]
             (if (empty? args)
               raw-normals
               (filter #(<= 0 (dot (first args) (first %)))))))
(defmethod normals :circle [shape & args]
             [(struct normal-struct (unit (first args)) 0)])
(println 1)
(defstruct projection-struct :start :stop :start-points :stop-points)
(println ((fn [m] (loop [q m accum []]
                   (condp = (- 4 (first q))
                       1 (if (odd? (first q))
                           [ accum]
                           (recur (rest q) (conj accum 4)))
                       2 (if (odd? (first q))
                           (recur (rest q) (conj accum 2))
                           (recur (rest q) (conj accum 5)))
                       3 (if (odd? (first q))
                           (recur (rest q) (conj accum 3))
                           (recur (rest q) (conj accum 6)))
                       accum))) [1 2 3 4]))
(println 1)

(defn search [condition shape plane]
  "Assuming shape is a polygon with it's vertices listed in a counterclockwise order under :points, condition is <= or >=,
   and line is a unit vector then this function returns a vector that starts with the smallest projection of all the points
   on the shape, and the points with that projection(could be two if a side is perpendicular to the line.
   "
  (loop [accum 0 step (inc (int  (/ (count (:points shape)) 2))) from -1]
    (condp condition (dot plane (accum (:points shape)))
      
      (dot plane ((mod (+ accum step) (count (:points shape))) (:points shape)))
      (if (= from (+ accum step))
        [(dot plane ((mod (+ accum step) (count (:points shape))) (:points shape)))
         (from (:points shape)) (accum (:points shape))]
        (recur (mod (+ accum step) (count (:points shape))) (inc (int (/ step 2))) accum)
        )

      (dot plane ((mod (- accum step) (count (:points shape))) (:points shape)))
      (if (= from (- accum step))
        [(dot plane ((mod (- accum step) (count (:points shape))) (:points shape)))
         (from (:points shape)) (accum (:points shape))]
        (recur (mod (- accum step) (count (:points shape))) (inc (int (/ step 2))) accum)
        )
      
      [(dot plane (accum (:points shape)))
       (accum (:points shape))])))

(defmulti projection #(:type (meta %)))
(println (matrix 1 2))
(println "***")
(println (matrix 3 4))
(println (dot (matrix 1 2) (matrix 3 4)))
(defmethod projection :polygon [shape plane]
           (let [front (search <= shape plane)
                 back (search >= shape plane)]
             (struct projection-struct (first front) (first back) (rest front) (rest back))))
(println 3)
(defmethod projection :circle [shape plane]
           (let [plane-unit (unit plane)]
             (struct projection-struct
                     (- (dot (:center shape) plane) (:radius shape))
                     (+ (dot (:center shape) plane) (:radius shape))
                     [(- (:center shape) (* (:radius shape) plane-unit))]
                     [(+ (:center shape) (* (:radius shape) plane-unit))])))
(println 1)
(defmulti farside-distance #(:type (meta %)))
(defmethod farside-distance :polygon [shape origin]
           (loop [points (:points shape) result 0]
             (if (empty? points)
               result
               (recur (rest points) (max result (length (- (first points) origin)))))))
(defmethod farside-distance :circle [shape origin]
           (- (+ (:center shape) (:radius shape)) origin))

(defmethod render :polynomial [shape transform g]
           (.setColor g Color/white)
           (doseq [i (range 1 (count (:points shape)))]
             (let [point (* transform (i (:points shape)))
                   last-point (* transform ((dec i) (:points shape)))]
               (.drawLine g (x last-point) (y last-point) (x point) (y point))))
           (let [point (* transform (first (:points shape)))
                 last-point (* transform (last (:points shape)))]
             (.drawLine g (x last-point) (y last-point) (x point) (y point))))

(defmethod render :circle [shape transform g]
           (.setColor g Color/white)
           (let [center (* transform (:center shape))]
             (.drawOval g (- (x center) (:radius shape)) (- (y center) (:radius shape)) (:radius shape) (:radius shape))))
           
(println 1)
