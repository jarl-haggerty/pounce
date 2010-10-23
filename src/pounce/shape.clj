(ns pounce.shape
  (:use pounce.math))

(defstruct shape :center :mass :points :normals)

(defn create-shape [mass & points]
  (let 
    [normals
     (loop [toProcess points lastPoint nil normals []]
       (if (empty? toProcess)
         (conj normals (normal (- (first points) (last points))))
         (if (nil? lastPoint)
           (recur (rest toProcess) (first toProcess) normals)
             (recur (rest toProcess) (first toProcess) (conj normals (normal (- (first toProcess) lastPoint)))))))]
    (struct shape (/ (reduce #(+ %1 %2) points) (count points)) mass points normals)))