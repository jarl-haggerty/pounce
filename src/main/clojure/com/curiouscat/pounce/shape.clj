(ns com.curiouscat.pounce.shape
  (:import java.awt.Color)
  (:refer-clojure :exclude [+ - * / < <= > >= max-key min-key])
  (:use com.curiouscat.pounce.math.math
        com.curiouscat.pounce.math.matrix
        com.curiouscat.pounce.render
        clojure.test))

(defn polygon [raw-mass & raw-points]
  (let [[mass points] (if (number? raw-mass) [raw-mass raw-points] [positive-infinity (cons raw-mass raw-points)])]
    (with-meta {:center (/ (reduce #(+ %1 %2) points) (count points))
                :mass mass
                :points points
                :normals (map #(normal (- %1 %2)) (conj (vec (rest points)) (first points)) points)}
      {:type :polygon})))

(defn circle
  ([center radius] (circle positive-infinity center radius))
  ([mass center radius]
     (with-meta  {:center center :mass mass :radius radius} {:type :circle})))

(defmulti normals (fn [shape & args] #(:type (meta shape))))

(defmethod normals :polygon
  ([shape] (map #(hash-map :normal %1 :side [%2 %3])
                (:normals shape)
                (:points shape)
                (conj (vec (rest (:points shape))) (first (:points shape)))))
  ([shape direction]
     (filter #(<= 0 (dot (normals shape) direction)))))

(defmethod normals :circle [shape direction]
           [{:normal (unit direction) :side 0}])

(defmulti projection (fn [shape plane] (:type (meta shape))))

(defmethod projection :polygon [shape plane]
           (let [points (circular-vector (:points shape))
                 search (fn [condition]
                            (loop [accum 0 step (ceil (/ (count (:points shape)) 2)) from nil]
                              (condp condition (dot plane (points accum))
                                
                                (dot plane (points (+ accum step)))
                                (if (= from (+ accum step))
                                  [(dot plane (points (+ accum step)))
                                   (points from) (points accum)]
                                  (recur (+ accum step) (ceil (/ step 2)) accum))

                                (dot plane (points (- accum step)))
                                (if (= from (- accum step))
                                  [(dot plane (points (- accum step)))
                                   (points from) (points accum)]
                                  (recur (- accum step) (ceil (/ step 2)) accum))
                                
                                [(dot plane (points accum))
                                 (points accum)])))
                 front (search <= shape plane)
                 back (search >= shape plane)]
           {:start (first front) :stop (first back) :start-points (rest front) :stop-points (rest back)}))

(defmethod projection :circle [shape plane]
           (let [plane-unit (unit plane)]
             {:start (- (dot (:center shape) plane) (:radius shape))
              :stop (+ (dot (:center shape) plane) (:radius shape))
              :start-points [(- (:center shape) (* (:radius shape) plane-unit))]
              :stop-points [(+ (:center shape) (* (:radius shape) plane-unit))]}))

(defmulti farside-distance (fn [shape origin] (:type (meta shape))))

(defmethod farside-distance :polygon [shape origin]
           (apply max (map #(length (- % origin)))))

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

(defmethod multiply [:transform :polynomial] [x y]
           (apply polygon (:mass y) (map #(* x %) (:points y))))

(defmethod multiply [:transform :circle] [x y]
           (apply polygon (:mass y) (* x (:center y)) (:radius y)))

(defmethod add [:polygon :matrix] [x y] (apply polygon (:mass x) (map #(+ y %) (:points x))))
(defmethod add [:circle :matrix] [x y] (apply polygon (:mass x) (+ y (:center x)) (:radius x)))
(defmethod add [:matrix :polygon] [x y] (apply polygon (:mass y) (map #(+ x %) (:points y))))
(defmethod add [:matrix :circle] [x y] (apply polygon (:mass y) (+ x (:center y)) (:radius y)))

(deftest shape-polygon-1
  (is (= (apply polygon (map matrix [[0 0] [1 0] [1 1] [0 1]]))
         {:center {:data [1/2 1/2] :height 2 :width 1}
          :mass positive-infinity
          :points (map matrix [[0 0] [1 0] [1 1] [0 1]])
          :normals (map matrix [[0 -1] [1 0] [0 1] [-1 0]])})))
(deftest shape-polygon-2
  (is (= (apply polygon 10 (map matrix [[0 0] [1 0] [1 1] [0 1]]))
         {:center {:data [1/2 1/2] :height 2 :width 1}
          :mass 10
          :points (map matrix [[0 0] [1 0] [1 1] [0 1]])
          :normals (map matrix [[0 -1] [1 0] [0 1] [-1 0]])})))
(deftest shape-circle-1
  (is (= (circle (matrix 0 0) 5) {:mass positive-infinity :center (matrix 0 0) :radius 5})))
(deftest shape-circle-2
  (is (= (circle 10 (matrix 0 0) 5) {:mass 10 :center (matrix 0 0) :radius 5})))
