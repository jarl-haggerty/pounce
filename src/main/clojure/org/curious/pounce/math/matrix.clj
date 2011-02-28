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

(defprotocol Vector2Protocol
  (x [this])
  (y [this]))
(deftype Vector2 [x-component y-component]
  Object
  (equals [this that] (and (math/eps= (x this) (x that))
                           (math/eps= (y this) (y that))))
  (toString [this] (str "[" x-component ", " y-component "]"))
  Vector2Protocol
  (x [this] x-component)
  (y [this] y-component))

(defprotocol Matrix2x2Protocol
  (ul [this])
  (ll [this])
  (ur [this])
  (lr [this]))
(deftype Matrix2x2 [ul-component ll-component ur-component lr-component]
  Object
  (equals [this that] (and (math/eps= (x this) (x that))
                           (math/eps= (y this) (y that))))
  (toString [this] (str "[[" ul-component ", " ll-component "], [" ur-component ", " lr-component "]]"))
  Matrix2x2Protocol
  (ul [this] ul-component)
  (ll [this] ll-component)
  (ur [this] ur-component)
  (lr [this] lr-component))

(defn rotate [R v]
  (Vector2. (+ (* (ul R) (x v)) (* (ur R) (y v)))
            (+ (* (ll R) (x v)) (* (lr R) (y v)))))
(defn compose [R1 R2]
  (Matrix2x2. (+ (* (ul R1) (ul R2)) (* (ur R1) (ll R2)))
              (+ (* (ll R1) (ul R2)) (* (lr R1) (ll R2)))
              (+ (* (ul R1) (ur R2)) (* (ur R1) (lr R2)))
              (+ (* (ll R1) (ur R2)) (* (lr R1) (lr R2)))))
(defn add [v w]
  (Vector2. (+ (x v) (x w)) (+ (y v) (y w))))
(defn dot [v w]
  (+ (* (x v) (x w))) (* (y v) (y w)))
(defn cross [v w]
  (- (* (x v) (y w)) (* (y v) (x w))))
(defn length-squared [v]
  (dot v v))
(defn length [v]
     (Math/sqrt (length-squared v)))
(defn unit [v]
     (let [len (length v)]
       (Vector2. (/ (x v) len) (/ (y v) len))))
(defn rotation [theta]
  (Matrix2x2. (math/cos theta) (math/sin theta) (- (math/sin theta)) (aset-double 1 1 (math/cos theta))))

(def swap1 (make-array Double/TYPE 2 2))
(def swap2 (make-array Double/TYPE 2 2))
(def swap3 (make-array Double/TYPE 2 2))

(defn get-swap-x [swap] (aget swap 0 0))
(defn set-swap-x [swap arg] (aset-float swap 0 0 arg))
(defn get-swap-y [swap] (aget swap 0 1))
(defn get-swap-y [swap arg] (aset-float swap 0 1 arg))
(defn get-swap-ul [swap] (aget swap 0 0))
(defn get-swap-ul [swap arg] (aset-float swap 0 0 arg))
(defn get-swap-ll [swap] (aget swap 0 1))
(defn get-swap-ll [swap arg] (aset-float swap 0 1 arg))
(defn get-swap-ur [swap] (aget swap 1 0))
(defn get-swap-ur [swap arg] (aset-float swap 1 0 arg))
(defn get-swap-lr [swap] (aget swap 1 1))
(defn get-swap-lr [swap arg] (aset-float swap 1 1 arg))

(defn swap-to-vector [swap] `(Vector2. (get-swap-x swap) (get-swap-y swap)))
(defn swap-to-matrix [swap] `(Matrix2x2. (get-swap-ul swap) (get-swap-ll swap) (get-swap-ur swap) (get-swap-lr swap)))
(defn matrix*vector=swap [R v swap]
  (set-swap-x swap (+ (* (ul R) (x v)) (* (ur R) (y v))))
  (set-swap-y swap (+ (* (ll R) (x v)) (* (lr R) (y v)))))
(defn compose-to-swap [R1 R2 swap]
  (set-swap-ul swap (+ (* (ul R1) (ul R2)) (* (ur R1) (ll R2))))
  (set-swap-ll swap (+ (* (ll R1) (ul R2)) (* (lr R1) (ll R2))))
  (set-swap-ur swap (+ (* (ul R1) (ur R2)) (* (ur R1) (lr R2))))
  (set-swap-lr swap (+ (* (ll R1) (ur R2)) (* (lr R1) (lr R2)))))
(defn vector+vector=swap [v w swap]
  (set-swap-x swap (+ (x v) (x w)))
  (set-swap-y swap (+ (y v) (y w))))
(defn vector+swap=vector [v swap]
  (Vector2. (+ (x v) (get-swap-x swap)) (+ (y v) (get-swap-y swap))))
(defn unit=swap [v swap]
  (let [len (length v)]
    (set-swap-x swap (/ (x v) len))
    (set-swap-y swap (/ (y v) len))))

(defrecord Transformation [translation rotation])

(defn transformation
  "Creates a transform from the specified displacement and rotation angle in radians."
  ([x y theta] (transformation (Vector2. x y) theta))
  ([translation theta] (Transformation. translation (if (number? theta)
                                                         (rotation theta)
                                                         theta))))

(def
 ^{:doc "The identity transform."}
 identity-transformation (transformation 0 0 0))

(defn transform [t v]
  (matrix*vector=swap (:rotation t) v swap1)
  (vector+swap=vector (:translation t) swap1))

(defn vec2 [arg]
  (if (instance? Vector2 arg)
    arg
    (Vector2. (first arg) (second arg))))

(defn mat2x2 [arg]
  (if (instance? Vector2 arg)
    arg
    (Vector2. (-> arg first first) (-> arg first second) (-> arg second first) (-> arg second second))))

(deftype Matrix [data]
  Object
  (equals [this that] (and (= (rows this) (rows that))
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
  (toString [this] (let [builder (StringBuilder.)]
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
