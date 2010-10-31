(ns pounce.math.transform
  (:use pounce.math.matrix))

(defstruct transformation-struct :type :translation :rotation)

(defn transformation [translation & rotation] 
  (struct transformation-struct 
    :transformation translation 
    (if (empty? rotation) 
      (scalar-matrix 2) 
      (if (number? (first rotation))
        (rotation-matrix (first rotation))
        (first rotation)))))

(def identity-transform
  (transformation 
    (zero 2) 
    (scalar-matrix 2)))

(defn transform [trans point] 
  (+ (:translation trans)
    (* (:rotation trans) point)))
(defn translate [original trans] 
  (transformation (-> original :translation (+ trans)) (:rotation original)))
(defn rotate [original point angle] 
  (transformation (+ (* angle (-> original :translation (- point))) point) (-> original :rotation (* angle))))
