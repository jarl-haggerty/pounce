(ns org.curious.pounce.math.matrix
  "Defines the matrix and transform data structures and the functions to manipulate them."
  (:refer-clojure :exclude [set])
  (:require [org.curious.pounce.math.core :as math]))

(defprotocol Table
  (data [this])
  (rows [this])
  (columns [this]))

(def add)
(def mul)

(deftype Matrix [array]
  Object
  (equals [this that] (and (= (rows this) (rows that))
                           (= (columns this) (columns that))
                           (loop [row (int 0)]
                             (if (< row (rows this))
                               (if (loop [column (int 0)]
                                     (if (< column (columns this))
                                       (if (math/eps= (aget (data this) row column) (aget (data that) row column))
                                         (recur (unchecked-inc column))
                                         false)
                                       true))
                                 (recur (unchecked-inc row))
                                 false)
                               true))))
  (toString [this] (let [builder (StringBuilder.)]
                     (.append builder "[")
                     (loop [column (int 0)]
                       (when (< column (columns this))
                         (loop [row (int 0)]
                           (when (< row (rows this))
                             (.append builder (aget (data this) row column))
                             (if (not (and (= column (dec (columns this)))
                                           (= row (dec (rows this)))))
                               (.append builder " "))
                             (recur (unchecked-inc row))))
                         (recur (unchecked-inc column))))
                     (.append builder "] rows: ")
                     (.append builder (rows this))
                     (.append builder " columns: ")
                     (.append builder (columns this))
                     (.append builder "")
                     (.toString builder)))
  Table
  (data [this] array)
  (rows [this] (alength (data this)))
  (columns [this] (alength (aget (data this) 0))))

(defn clone [this]
  (let [new-data (make-array Float/TYPE (rows this) (columns this))]
    (loop [row (int 0)]
      (when (< row (rows this))
        (loop [column (int 0)]
          (when (< column (columns this))
            (aset-float new-data row column (aget (data this) row column))
            (recur (unchecked-inc column))))
        (recur (unchecked-inc row))))
    (Matrix. new-data)))

(defn set [this row column source]
  (cond (number? source) (aset-float (data this) row column source)
        (instance? Matrix source) (loop [row-index (int 0)]
                                    (when (< row-index (columns source))
                                      (loop [column-index (int 0)]
                                        (when (< column-index (rows source))
                                          (aset-float (data this) (unchecked-add row row-index) (unchecked-add column column-index)
                                                      (aget (data source) row-index column-index))
                                          (recur (unchecked-inc column-index))))
                                      (recur (unchecked-inc row-index)))))
  this)

(defn- add-in-place [this that]
  (cond (number? that) (loop [row (int 0)]
                         (when (< row (rows this))
                           (loop [column (int 0)]
                             (when (< column (columns this))
                               (aset-float (data this) row column (+ (aget ^floats (data this) row column) that))
                               (recur (unchecked-inc column))))
                           (recur (unchecked-inc row))))
        (instance? Matrix that) (loop [row (int 0)]
                                  (when (< row (rows this))
                                    (loop [column (int 0)]
                                      (when (< column (columns this))
                                        (aset-float (data this) row column (+ (aget ^floats (data this) row column) (aget ^floats (data that) row column)))
                                        (recur (unchecked-inc column))))
                                    (recur (unchecked-inc row)))))
  this)

(defn add
  ([this that]
     (let [new-data (make-array Float/TYPE (rows this) (columns this))]
       (cond (number? that) (loop [row (int 0)]
                              (when (< row (rows this))
                                (loop [column (int 0)]
                                  (when (< column (columns this))
                                    (aset-float new-data row column (+ (aget ^floats (data this) row column) that))
                                    (recur (unchecked-inc column))))
                                (recur (unchecked-inc row))))
             (instance? Matrix that) (loop [row (int 0)]
                                      (when (< row (rows this))
                                        (loop [column (int 0)]
                                          (when (< column (columns this))
                                            (aset-float new-data row column (+ (aget (data this) row column) (aget (data that) row column)))
                                            (recur (unchecked-inc column))))
                                        (recur (unchecked-inc row)))))
       (Matrix. new-data)))
  ([this that & others]
     (reduce add-in-place (add this that) others)))

(defn- sub-in-place [this that]
  (cond (number? that) (loop [row (int 0)]
                         (when (< row (rows this))
                           (loop [column (int 0)]
                             (when (< column (columns this))
                               (aset-float (data this) row column (- (aget ^floats (data this) row column) that))
                               (recur (unchecked-inc column))))
                           (recur (unchecked-inc row))))
        (instance? Matrix that) (loop [row (int 0)]
                                  (when (< row (rows this))
                                    (loop [column (int 0)]
                                      (when (< column (columns this))
                                        (aset-float (data this) row column (- (aget ^floats (data this) row column) (aget ^floats (data that) row column)))
                                        (recur (unchecked-inc column))))
                                    (recur (unchecked-inc row)))))
  this)

(defn sub
  ([this]
     (cond (number? this) (- this) 
           (instance? Matrix this) (let [new-data (make-array Float/TYPE (rows this) (columns this))]
                                     (loop [row (int 0)]
                                       (when (< row (rows this))
                                         (loop [column (int 0)]
                                           (when (< column (columns this))
                                             (aset-float new-data row column (- (aget ^floats (data this) row column)))
                                             (recur (unchecked-inc column))))
                                         (recur (unchecked-inc row))))
                                     (Matrix. new-data))))
  ([this that]
     (add-in-place (sub that) this))
    ([this that & others]
       (reduce sub-in-place (sub this that) others)))

(defn mul
  [this that]
  (cond (number? that) (let [new-data (make-array Float/TYPE (rows this) (columns this))]
                         (loop [row (int 0)]
                           (when (< row (rows this))
                             (loop [column (int 0)]
                               (when (< column (columns this))
                                 (aset-float new-data row column (* (aget ^floats (data this) row column) that))
                                 (recur (unchecked-inc column))))
                             (recur (unchecked-inc row))))
                         (Matrix. new-data))
        (instance? Matrix that) (let [new-data (make-array Float/TYPE (rows this) (columns that))]
                                  (loop [row (int 0)]
                                    (when (< row (rows this))
                                      (loop [column (int 0)]
                                        (when (< column (columns that))
                                          (loop [index (int 0)]
                                            (when (< index (columns this))
                                              (aset-float new-data row column (+ (aget ^float new-data row column)
                                                                                 (* (aget ^float (data this) row index)
                                                                                    (aget ^float (data that) index column))))
                                              (recur (unchecked-inc index))))
                                          (recur (unchecked-inc column))))
                                      (recur (unchecked-inc row))))
                                  (Matrix. new-data))))

(defn div [this that] (mul this (/ that)))

(defn x [this] (aget ^float (data this) 0 0))
(defn y [this] (aget ^float (data this) 1 0))

(defn dot [this that] (loop [row (int 0) accum 0]
                        (if (< row (rows this))
                          (recur (unchecked-inc row) (+ accum (* (aget ^float (data this) row 0)
                                                                 (aget ^float (data that) row 0))))
                          accum)))

(defn cross [this that] (- (* (x this) (y that)) (* (x that) (y this))))

(defn length-squared [this] (dot this this))

(defn length [this] (math/sqrt (length-squared this)))

(defn unit [this] (div this (length this)))

(defn create
  "Returns M if it's a matrix, otherwise (matrix M) is returned."
  [& args] (cond (instance? Matrix (first args)) (first args)
                 (sequential? (first args)) (let [new-data (make-array Float/TYPE (count (first args)) (count args))]
                                              (dorun (map-indexed (fn [column column-data]
                                                                    (dorun (map-indexed (fn [row value]
                                                                                          (aset-float new-data row column value))
                                                                                        column-data)))
                                                                  args))
                                              (Matrix. new-data))
                 (number? (first args)) (let [new-data (make-array Float/TYPE (count args) 1)]
                                          (dorun (map-indexed (fn [row value]
                                                                (aset-float new-data row 0 value))
                                                              args))
                                          (Matrix. new-data))))

(defn rotation-matrix
  "Calculates the rotation matrix for theta radians"
  [theta] (Matrix. (doto (make-array Float/TYPE 2 2)
                     (aset-float 0 0 (math/cos theta))
                     (aset-float 1 0 (math/sin theta))
                     (aset-float 0 1 (- (math/sin theta)))
                     (aset-float 1 1 (math/cos theta)))))

(defn rotate [this theta]
  (create (- (* (x this) (math/cos theta)) (* (y this) (math/sin theta)))
          (+ (* (x this) (math/sin theta)) (* (y this) (math/cos theta)))))

(defrecord Transformation [translation rotation])

(defn transformation
  "Creates a transform from the specified displacement and rotation angle in radians."
  ([x y rotation] (transformation (create x y) rotation))
  ([translation rotation] (Transformation. translation (rotation-matrix rotation))))

(def
 ^{:doc "The identity transform."}
 identity-transform (transformation 0 0 0))

(defn transform [this t]
  (add (mul (:rotation t) this) (:translation t)))
