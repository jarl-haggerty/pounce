(ns com.curious.pounce.math.matrix
  "Defines the matrix and transform data structures and the functions to manipulate them."
  (:use com.curious.pounce.math.math))

(defprotocol MatrixProtocol
  (add [this that])
  (add-in-place [this that])
  (sub [this] [this that])
  (sub-in-place [this] [this that])
  (mul [this that])
  (div [this that])
  (get-value [this row column])
  (set-value [this row column value])
  (x [this])
  (y [this])
  (cross [this that])
  (dot [this that])
  (length [this])
  (length-squared [this])
  (unit [this]))

(defrecord Matrix [data height width]
  Object
  (equals [this that] (and (= height (get-height that))
                           (= width (get-hidth that))
                           (loop [index (int 0)]
                             (if (< index (alength data))
                               (if (eps= (aget ^floats data index) (aget ^floats (getData that) index))
                                 (recur (unchecked-inc index))
                                 false)
                               true))))
  Container2D
  (getData [this] data)
  (getHeight [this] height)
  (getWidth [this] width)
  (getValue [this row column] (aget ^floats data (unchecked-add row (unchecked-multiply column height))))
  (setValue [this row column value] (aset-float data (unchecked-add row (unchecked-multiply column height)) value))
  (add [this that]
       (cond
        (number? that) (Matrix. (amap data index new-data (add (aget ^floats data index) that)) height width)
        (extends? Composeable that) (Matrix. (amap data index new-data (add (aget ^floats data index) (aget ^floats (getData that) index))) height width)))
  (sub [this] (Matrix. (amap data index new-data (unchecked-negate (aget ^floats data index))) height width))
  (sub [this that] (Matrix. (amap data index new-data (unchecked-subtract (aget ^floats data index) (aget ^floats (getData that) index))) height width))
  (sub-in-place [this]
                (loop [index (int 0)]
                  (when (< index (alength data))
                    (aset-float data index (unchecked-negate (aget ^floats data index)))
                    (recur (unchecked-inc index)))))
  (mul [this that] (cond
                    (number? that) (Matrix. (amap data index new-data (unchecked-multiply (aget ^floats data index) (aget ^floats (getData that) index))) height width)
                    :else (let [result (Matrix. (float-array (unchecked-multiply height (getWidth y))) height (getWidth y))]
                            (loop [r (int 0)]
                              (when (< r height)
                                (loop [c (int 0)]
                                  (when (< c (getWidth y))
                                    (loop [i (int 0)]
                                      (when (< i width)
                                        (setValue result r c
                                                  (+ (mget result r c)
                                                     (* (mget this r i)
                                                        (mget that i c))))
                                        (recur (unchecked-inc i))))
                                    (recur (unchecked-inc c))))
                                (recur (unchecked-inc r))))
                            result)))
  (div [this that] (Matrix. (amap data index new-data (unchecked-divide (aget ^floats data index) that)) height width))
  (x [this] (getValue this 0 0))
  (y [this] (getValue this 1 0))
  (cross [this that] (unchecked-subtract (unchecked-multiply (x this) (y that))
                                         (unchecked-multiply (x that) (y this))))
  (dot [this that] (unchecked-add (unchecked-multiply (x this) (x that))
                                  (unchecked-multiply (y this) (y that))))
  (length [this] (sqrt (lengthSquared this)))
  (lengthSquared [this] (areduce data index accum 0 (unchecked-add accum (pow (aget ^floats data index) 2))))
  (unit [this]
        (let [len (length this)]
          (Matrix. (amap data index new-data (unchecked-divide (aget ^floats data index) len)))))
  (normal [this] (let [len (length this)]
                   (Matrix. (doto (float-array 2)
                              (aset-float 0 (y this))
                              (aset-float 1 (unchecked-negate (x this)))))))
  Transformable
  (transform [this trans] (add (mul this (getRotation trans)) (getTranslation trans)))
  (translate [this translation] (add this translation))
  (rotate [this rotation] (mul this rotation)))

(defn matrix [& args]
  (Matrix. (float-array args) (count args) 1))
(defn mat
  "Returns M if it's a matrix, otherwise (matrix M) is returned."
  [M & args] (cond
              (extends? MatrixProtocol M) M
              (sequential? M) (apply matrix M)
              :else (apply matrix M args)))

(defn rotation-matrix
  "Calculates the rotation matrix for theta radians"
  [theta] (Matrix. (doto (float-array 4)
                     (aset-float 0 (cos theta))
                     (aset-float 1 (sin theta))
                     (aset-float 2 (unchecked-negate (sin theta)))
                     (aset-float 3 (cos theta)))
                   2 2))

(defprotocol TransformationProtocol
  (getTranslation [this])
  (getRotation [this]))

(defprotocol Transformable
  (transform [this trans])
  (translate [this translation])
  (rotate [this rotation]))

(deftype Transformation [translation rotation]
  Object
  (equals [this that] (and (equals translation (getTranslation that))
                           (equals rotation (getRotation that))))
  TransformationProtocol
  (getTranslation [this] translation)
  (getRotation [this] rotation))

(defn transformation
  "Creates a transform from the specified displacement and rotation angle in radians."
  ([x y angle] (transform (matrix x y) angle))
  ([displacement angle] (Transformation. displacement angle)))

(def
 ^{:doc "The identity transform."}
 identity-transform (transform 0 0 0))
