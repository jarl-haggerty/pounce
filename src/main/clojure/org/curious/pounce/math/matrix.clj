(comment
  Copyright 2010 Jarl Haggerty

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0
  
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.)


(ns org.curious.pounce.math.matrix
  "Defines the matrix and transform data structures and the functions to manipulate them."
  (:refer-clojure :exclude [get set])
  (:require [org.curious.pounce.math.core :as math]))

(defprotocol Table
  (rows [this])
  (columns [this])
  (transpose [this])
  (get [this row column])
  (set [this row column source])
  (batch-set [this row column source])
  (clone [this]))

(def primitive Double/TYPE)
(def aset-primitive aset-double)

(defn- matrix-equals [this that] (and (= (rows this) (rows that))
                                      (= (columns this) (columns that))
                                      (loop [row (int 0)]
                                        (if (< row (rows this))
                                          (if (loop [column (int 0)]
                                                (if (< column (columns this))
                                                  (if (math/eps= (get this row column) (get that row column))
                                                    (recur (unchecked-inc column))
                                                    false)
                                                  true))
                                            (recur (unchecked-inc row))
                                            false)
                                          true))))

(defn- matrix-string [this] (let [builder (StringBuilder.)]
                              (.append builder "[")
                              (loop [column (int 0)]
                                (when (< column (columns this))
                                  (loop [row (int 0)]
                                    (when (< row (rows this))
                                      (.append builder (get this row column))
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

(def add)
(def mul)

(deftype Matrix [data]
  Object
  (equals [this that] (matrix-equals this that))
  (toString [this] (matrix-string this))
  Table
  (rows [this] (alength data))
  (columns [this] (alength (aget data 0)))
  (get [this row column] (aget ^doubles data row column))
  (set [this row column source] (aset-double data row column source) this)
  (batch-set [this row column source]
             (loop [row-index (int 0)]
               (when (< row-index (columns source))
                 (loop [column-index (int 0)]
                   (when (< column-index (rows source))
                     (aset-double data (unchecked-add row row-index) (unchecked-add column column-index)
                                 (aget ^doubles (data source) row-index column-index))
                     (recur (unchecked-inc column-index))))
                 (recur (unchecked-inc row-index))))
             this)
  (clone [this] (let [new-data (make-array Double/TYPE (rows this) (columns this))]
                  (loop [row (int 0)]
                    (when (< row (rows this))
                      (loop [column (int 0)]
                        (when (< column (columns this))
                          (aset-double new-data row column (aget ^doubles data row column))
                          (recur (unchecked-inc column))))
                      (recur (unchecked-inc row))))
                  (Matrix. new-data))))

(deftype MultiMatrix [row-stride column-stride matrices]
  Object
  (equals [this that] (matrix-equals this that))
  (toString [this] (matrix-string this))
  Table
  (rows [this] (* row-stride (alength matrices)))
  (columns [this] (* column-stride (alength (aget matrices 0))))
  (get [this row column] (if-let [matrix (aget matrices (unchecked-divide row row-stride) (unchecked-divide column column-stride))]
                           (get matrix (unchecked-remainder row row-stride) (unchecked-remainder column column-stride))
                           0))
  (set [this row column source] (if-let [matrix (aget matrices (unchecked-divide row row-stride) (unchecked-divide column column-stride))]
                                  (aset-double matrix (unchecked-remainder row row-stride) (unchecked-remainder column column-stride) source)
                                  (let [new-data (make-array Double/TYPE row-stride column-stride)]
                                    (aset-double new-data (unchecked-remainder row row-stride) (unchecked-remainder column column-stride) source)
                                    (aset matrices (unchecked-divide row row-stride) (unchecked-divide column column-stride) (Matrix. new-data))))
       this)
  (batch-set [this row column source] (aset matrices row column source) this)
  (clone [this] (let [new-data (make-array Matrix (-> matrices alength) (-> matrices (aget 0) alength))]
                  (loop [row (int 0)]
                    (when (< row (-> matrices alength))
                      (loop [column (int 0)]
                        (when (< column (-> matrices (aget 0) alength))
                          (aset new-data row column (if-let [old (aget matrices row column)]
                                                      (clone old)
                                                      nil))
                          (recur (unchecked-inc column))))
                      (recur (unchecked-inc row))))
                  (MultiMatrix. row-stride column-stride new-data))))

(deftype TransposeMatrix [original]
  Object
  (equals [this that] (matrix-equals this that))
  (toString [this] (matrix-string this))
  Table
  (rows [this] (columns original))
  (columns [this] (rows original))
  (get [this row column] (get original column row))
  (set [this row column value] (set original column row value) this)
  (batch-set [this row column value] (batch-set original column row (TransposeMatrix. value)) this)
  (clone [this] (TransposeMatrix. (clone original))))

(deftype DiagonalMatrix [diagonal]
  Object
  (equals [this that] (matrix-equals this that))
  (toString [this] (matrix-string this))
  Table
  (rows [this] (alength diagonal))
  (columns [this] (alength diagonal))
  (get [this row column] (if (= row column)
                                  (aget ^doubles diagonal row)
                                  0))
  (set [this row column value] (if (= row column)
                                 (aset-double diagonal row value)
                                 (throw (Exception. "Cannot alter non-diagonal entries in diagonal matrix.")))
       this)
  (batch-set [this row column value] (throw (Exception. "batch-set not available for diagonal matrices.")) this)
  (clone [this] (DiagonalMatrix. (aclone diagonal))))

(defn multi-matrix [row-stride column-stride rows columns]
  (MultiMatrix. row-stride column-stride (make-array Matrix rows columns)))

(defn transpose [this]
  (TransposeMatrix. this))

(defn diagonal-matrix [& args]
  (let [data (make-array Double/TYPE (count args))]
    (doall (map-indexed #(aset-double data %1 %2) args))
    (DiagonalMatrix. data)))

(defn allocate-diagonal-matrix [size]
  (DiagonalMatrix. (make-array Double/TYPE size)))

(defn- add-in-place [this that]
  (cond (number? that) (loop [row (int 0)]
                         (when (< row (rows this))
                           (loop [column (int 0)]
                             (when (< column (columns this))
                               (set this row column (+ (get this row column) that))
                               (recur (unchecked-inc column))))
                           (recur (unchecked-inc row))))
        (instance? Matrix that) (loop [row (int 0)]
                                  (when (< row (rows this))
                                    (loop [column (int 0)]
                                      (when (< column (columns this))
                                        (set this row column (+ (get this row column) (get that row column)))
                                        (recur (unchecked-inc column))))
                                    (recur (unchecked-inc row)))))
  this)

(defn add
  ([this that]
     (let [new-data (make-array Double/TYPE (rows this) (columns this))]
       (cond (number? that) (loop [row (int 0)]
                              (when (< row (rows this))
                                (loop [column (int 0)]
                                  (when (< column (columns this))
                                    (aset-double new-data row column (+ (get this row column) that))
                                    (recur (unchecked-inc column))))
                                (recur (unchecked-inc row))))
             (instance? Matrix that) (loop [row (int 0)]
                                      (when (< row (rows this))
                                        (loop [column (int 0)]
                                          (when (< column (columns this))
                                            (aset-double new-data row column (+ (get this row column) (get that row column)))
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
                               (set this row column (- (get this row column) that))
                               (recur (unchecked-inc column))))
                           (recur (unchecked-inc row))))
        (instance? Matrix that) (loop [row (int 0)]
                                  (when (< row (rows this))
                                    (loop [column (int 0)]
                                      (when (< column (columns this))
                                        (set this row column (- (get this row column) (get that row column)))
                                        (recur (unchecked-inc column))))
                                    (recur (unchecked-inc row)))))
  this)

(defn sub
  ([this]
     (cond (number? this) (- this) 
           (instance? Matrix this) (let [new-data (make-array Double/TYPE (rows this) (columns this))]
                                     (loop [row (int 0)]
                                       (when (< row (rows this))
                                         (loop [column (int 0)]
                                           (when (< column (columns this))
                                             (aset-double new-data row column (- (get this row column)))
                                             (recur (unchecked-inc column))))
                                         (recur (unchecked-inc row))))
                                     (Matrix. new-data))))
  ([this that]
     (add-in-place (sub that) this))
    ([this that & others]
       (reduce sub-in-place (sub this that) others)))

(defn mul
  ([this that]
     (cond (number? that) (let [new-data (make-array Double/TYPE (rows this) (columns this))]
                            (loop [row (int 0)]
                              (when (< row (rows this))
                                (loop [column (int 0)]
                                  (when (< column (columns this))
                                    (aset-double new-data row column (* (get this row column) that))
                                    (recur (unchecked-inc column))))
                                (recur (unchecked-inc row))))
                            (Matrix. new-data))
           (extends? Table (class that)) (let [new-data (make-array Double/TYPE (rows this) (columns that))]
                                           (loop [row (int 0)]
                                             (when (< row (rows this))
                                               (loop [column (int 0)]
                                                 (when (< column (columns that))
                                                   (loop [index (int 0)]
                                                     (when (< index (columns this))
                                                       (aset-double new-data row column (+ (aget ^doubles new-data row column)
                                                                                          (* (get this row index)
                                                                                             (get that index column))))
                                                       (recur (unchecked-inc index))))
                                                   (recur (unchecked-inc column))))
                                               (recur (unchecked-inc row))))
                                           (Matrix. new-data))))
  ([this that & more]
     (reduce mul (mul this that) more)))

(defn div [this that] (mul this (/ that)))

(def zero (Matrix. (doto (make-array Double/TYPE 2 1)
                     (aset-double 0 0 0)
                     (aset-double 1 0 0))))
(defn x [this] (get this 0 0))
(defn y [this] (get this 1 0))

(defn dot [this that] (loop [row (int 0) accum 0]
                        (if (< row (rows this))
                          (recur (unchecked-inc row) (+ accum (* (get this row 0)
                                                                 (get that row 0))))
                          accum)))

(defn cross [this that] (- (* (x this) (y that)) (* (x that) (y this))))

(defn length-squared [this] (dot this this))

(defn length [this] (math/sqrt (length-squared this)))

(defn unit [this] (div this (length this)))

(defn column [& args]
  (let [new-data (make-array Double/TYPE (count args) 1)]
    (dorun (map-indexed (fn [row value]
                          (aset-double new-data row 0 value))
                        args))
    (Matrix. new-data)))

(defn row [& args]
  (let [new-data (make-array Double/TYPE 1 (count args))]
    (dorun (map-indexed (fn [column value]
                          (aset-double new-data 0 column value))
                        args))
    (Matrix. new-data)))

(defn mat [arg]
  (if (instance? Matrix arg)
    arg
    (apply column arg)))

(defn create
  "Returns the argument if it's a matrix, a matrix with the arguments as rows if they are sequences, or a column matrix if the arguments are numbers."
  [& args] (cond (instance? Matrix (first args)) (first args)
                 (sequential? (first args)) (let [new-data (make-array primitive (count (first args)) (count args))]
                                              (dorun (map-indexed (fn [column column-data]
                                                                    (dorun (map-indexed (fn [row value]
                                                                                          (aset-double new-data row column value))
                                                                                        column-data)))
                                                                  args))
                                              (Matrix. new-data))
                 (number? (first args)) (column args)))

(defn allocate
  "Returns a matrix with the specified rows and columns"
  ([rows] (allocate rows 1))
  ([rows columns] (Matrix. (make-array Double/TYPE rows columns))))

(defn rotation-matrix
  "Calculates the rotation matrix for theta radians"
  [theta] (Matrix. (doto (make-array Double/TYPE 2 2)
                     (aset-double 0 0 (math/cos theta))
                     (aset-double 1 0 (math/sin theta))
                     (aset-double 0 1 (- (math/sin theta)))
                     (aset-double 1 1 (math/cos theta)))))

(defn rotate [this theta]
  (column (- (* (x this) (math/cos theta)) (* (y this) (math/sin theta)))
          (+ (* (x this) (math/sin theta)) (* (y this) (math/cos theta)))))

(defrecord Transformation [translation rotation])

(defn transformation
  "Creates a transform from the specified displacement and rotation angle in radians."
  ([x y rotation] (transformation (column x y) rotation))
  ([translation rotation] (Transformation. translation (if (number? rotation)
                                                         (rotation-matrix rotation)
                                                         rotation))))

(def
 ^{:doc "The identity transform."}
 identity-transformation (transformation 0 0 0))

(defn transform [this t]
  (add-in-place (mul (:rotation t) this) (:translation t)))

(defn gauss-seidel
  ([A b] (gauss-seidel A b math/eps))
  ([A b metric] (gauss-seidel A b metric math/negative-infinity math/positive-infinity))
  ([A b lower-bound upper-bound] (gauss-seidel A b (apply column (repeat (rows b) 0)) math/eps lower-bound upper-bound))
  ([A b metric lower-bound upper-bound] (gauss-seidel A b (apply column (repeat (rows b) 0)) metric lower-bound upper-bound))
  ([A b initial-x metric lower-bound upper-bound]
     (let [x (clone initial-x)
           metric-function (if (integer? metric)
                             (fn [iteration max-change] (< iteration metric))
                             (fn [iteration max-change] (< metric max-change)))]
       (loop [iteration (int 0) max-change Float/POSITIVE_INFINITY]
         (when (metric-function iteration max-change)
           (let [new-max-change (loop [row (int 0) max-change 0]
                                  (if (< row (rows A))
                                    (let [old-value (get x row 0)
                                          new-value (-> (/ (- (get b row 0)
                                                                (loop [column (unchecked-inc row) accum 0]
                                                                  (if (< column (columns A))
                                                                    (recur (unchecked-inc column)
                                                                           (+ accum
                                                                              (* (get A row column) (get x column 0))))
                                                                    accum))
                                                                (loop [column (int 0) accum 0]
                                                                  (if (< column row)
                                                                    (recur (unchecked-inc column)
                                                                           (+ accum
                                                                              (* (get A row column) (get x column 0))))
                                                                    accum)))
                                                             (get A row row))
                                                          (max lower-bound)
                                                          (min upper-bound))
                                          change (math/abs (- new-value old-value))]
                                      (set x row 0 new-value)
                                      (recur (unchecked-inc row) (if (< max-change change) change max-change)))
                                    max-change))]
             (recur (unchecked-inc iteration) (float new-max-change)))))
       x)))
