(ns pounce.math.math
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key]))

(def positive-infinity Double/POSITIVE_INFINITY)
(def negative-infinity Double/NEGATIVE_INFINITY)
(def nan Double/NaN)
(def pi Math/PI)
(def *eps* 1e-10)

(defn is-infinite [input] (Double/isInfinite input))
(defn sin [input] (Math/sin (float input)))
(defn cos [input] (Math/cos input))
(defn pow [input power] (Math/pow input power))
(defn sqrt [input] (Math/sqrt input))
(defn abs [input] (if (is-infinite input) positive-infinity (Math/abs (float input))))
(defn ceil [x] (if (integer? x) x (inc (int x))))
(defn floor [x] (int x))
(defn round [x] (int (clojure.core/+ x 1/2)))
(defn circular-vector [coll] (fn [x] (nth coll (mod x (count coll)))))
(defn eps=
  ([x y] (eps= x y *eps*))
  ([x y eps] (or (clojure.core/= positive-infinity x y)
                 (clojure.core/= negative-infinity x y)
                 (clojure.core/< (abs (clojure.core/- x y)) eps))))

(defmulti add (fn [x y] [(:type (meta x)) (:type (meta y))]))
(defmethod add :default [x y] (clojure.core/+ x y))
(defn +
  ([] 0)
  ([x] x)
  ([x y] (add x y))
  ([x y & more]
    (reduce + (+ x y) more)))

(defmulti negate (fn [x] (:type (meta x))))
(defmethod negate :default [x] (clojure.core/- x))
(defn -
  ([] 0)
  ([x] (negate x))
  ([x y] (+ x (negate y)))
  ([x y & more]
    (reduce - (- x y) more)))

(defmulti multiply (fn [x y] [(:type (meta x)) (:type (meta y))]))
(defmethod multiply :default [x y] (clojure.core/* x y))
(defn *
  ([] 1)
  ([x] x)
  ([x y] (multiply x y))
  ([x y & more]
    (reduce * (* x y) more)))

(defmulti invert (fn [x] (:type (meta x))))
(defmethod invert :default [x] (clojure.core// x))
(defn /
  ([] 1)
  ([x] (invert x))
  ([x y] (* x (invert y)))
  ([x y & more]
    (reduce / (/ x y) more)))

(defmulti less-than (fn [x y] [(:type (meta x)) (:type (meta y))]))
(defmethod less-than :default [x y] (clojure.core/< x y))
(defn <
  ([] true)
  ([x] true)
  ([x y] (less-than x y))
  ([x y & more]
     (reduce < (< x y) more)))

(defmulti equal (fn [x y]
                  [(if (float? x)
                     :float
                     (if (integer? x)
                       :integer
                       (:type (meta x))))
                   (if (float? y)
                     :float
                     (if (integer? y)
                       :integer
                       (:type (meta y))))]))

(defmethod equal [:float :float] [x y] (eps= x y))
(defmethod equal [:float :integer] [x y] (eps= x y))
(defmethod equal [:integer :float] [x y] (eps= x y))
(defmethod equal :default [x y] (clojure.core/= x y))
(defn =
  ([x] true)
  ([x y] (equal x y))
  ([x y & more]
   (if (= x y)
     (if (next more)
       (recur y (first more) (next more))
       (= y (first more)))
     false)))
(defn seq=
  ([x] true)
  ([x y] (and (= (count x) (count y)) (every? identity (map #(= %1 %2) x y))))
  ([x y & more]
     (if (seq= x y)
       (if (next more)
         (recur y (first more) (next more))
         (seq= y (first more)))
       false)))

(defn not=
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more]
     (not (apply = x y more))))
(defn not-seq=
  ([x] false)
  ([x y] (not (seq= x y)))
  ([x y & more]
     (not (apply seq= x y more))))

(defn <=
  ([] true)
  ([x] true)
  ([x y] (or (= x y) (< x y)))
  ([x y & more]
      (reduce <= (<= x y) more)))

(defn >
  ([] true)
  ([x] true)
  ([x y] (< y x))
  ([x y & more]
      (reduce > (> x y) more)))

(defn >=
  ([] true)
  ([x] true)
  ([x y] (or (= x y) (> x y)))
  ([x y & more]
     (reduce >= (>= x y) more)))

(defn max-key
  ([k x] x)
  ([k x y] (if (> (k x) (k y)) x y))
  ([k x y & more]
   (reduce #(max-key k %1 %2) (max-key k x y) more)))

(defn min-key
  ([k x] x)
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
   (reduce #(min-key k %1 %2) (min-key k x y) more)))
