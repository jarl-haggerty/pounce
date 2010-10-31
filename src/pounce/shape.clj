(ns pounce.shape
  (:use pounce.math.core
        pounce.render))

(defstruct polygon-struct :center :mass :points :normals)
(defstruct circle-struct :center :mass :radius :)

(defn polygon [mass & points]
  (let 
    [normals
     (loop [toProcess points lastPoint nil normals []]
       (if (empty? toProcess)
         (conj normals (normal (- (first points) (last points))))
         (if (nil? lastPoint)
           (recur (rest toProcess) (first toProcess) normals)
             (recur (rest toProcess) (first toProcess) (conj normals (normal (- (first toProcess) lastPoint)))))))]
    (with-meta (struct shape (/ (reduce #(+ %1 %2) points) (count points)) mass points normals) {:type :polynomial})))

(defmulti normals #(:type (meta %)))
(defmethod normals :polygon [shape & args]
           (filter #(<= 0 (dot (first args) (first %)))
                   (map vector (:normals shape) (:points shape) (conj (rest (:points shape)) (first (:points shape))))))
(defmethod normals :circle [shape & args]
           (let [n (unit (first args))]
             (apply vector n (repeat (* (:radius shape) n) 2))))

(defmulti project #(:type (meta %)))
(defmethod project :polygon [shape plane]
           (let [search (fn [condition]
                          (loop [accum 0 step (inc (int  (/ (count (:points shape)) 2))) accum [] from nil]
                            (condp condition (dot plane (accum (:points shape)))
                              (mod (+ accum step) (count (:points shape)))
                              (if (= from ((+ accum step) (:points shape)))
                                [from (accum (:points shape))]
                                (recur (+ accum step)  (inc (int (/ step 2)))))
                              (mod (- accum step) (count (:points shape)))
                              (if (= from ((+ accum step) (:points shape)))
                                [from (accum (:points shape))]
                                (recur (- accum step) (inc (int (/ step 2)))))
                              [(accum (:points shape))])))
                 front (search <=)
                 back (search >=)]
             (apply vector (first front) (first back) (concat (rest front) (rest back)))))
(defmethod project :circle [shape plane]
           (map #(+ (dot (:center shape) plane) %) (:radius shape) (- (:radius shape))))

(defmulti farside-distance #(:type (meta %)))
(defmethod farside-distance :polygon [shape point]
           (loop [points (:points shape) result 0]
             (if (empty? points)
               result
               (recur (rest points) (max result (length (- (first points) center)))))))
(defmethod farside-distance :circle [shape point]
           (- (+ (:center shape) (:radius shape)) center))

(defmethod render :polynomial [shape transform g]
           (.setColor g Color/white)
           (doseq [i (range 1 (count (:points shape)))]
             (let [point (* transform (i (points shape)))
                   last-point (* transform ((dec i) (points shape)))]
               (.drawLine g (x last-point) (y last-point) (x point) (y point))))
           (let [point (* transform (first (:points shape)))
                 last-point (* transform (last (points shape)))]
             (.drawLine g (x last-point) (y last-point) (x point) (y point))))

(defmethod render :circle [shape transform g]
           (.setColor g Color/white)
           (let [center (* transform (:center shape))]
             (.drawOval g (- (x center) (:radius shape)) (- (y center) (:radius shape)) (:radius shape) (:radius shape))))
           
