(ns com.curiouscat.pounce.math.math
  (:refer-clojure :exclude [+ - * / < <= > >= max-key min-key])
  (:use clojure.test))

(def positive-infinity Double/POSITIVE_INFINITY)
(def negative-infinity Double/NEGATIVE_INFINITY)
(def pi Math/PI)
(def *eps* 1e-10)

(defn is-infinite [input] (Double/isInfinite input))
(defn sin [input] (Math/sin input))
(defn cos [input] (Math/cos input))
(defn pow [input power] (Math/pow input power))
(defn sqrt [input] (Math/sqrt input))
(defn abs [input] (Math/abs input))
(defn ceil [x] (if (integer? x) x (inc (int x))))
(defn floor [x] (int x))
(defn round [x] (int (clojure.core/+ x 1/2)))
(defn circular-vector [coll] (fn [x] (nth coll (mod x (count coll)))))
(defn eps=
  ([x y] (eps= x y *eps*))
  ([x y eps] (clojure.core/< (abs (clojure.core/- x y)) eps)))

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

(deftest math-infinite-test
  (is (is-infinite positive-infinity))
  (is (is-infinite negative-infinity))
  (is (not (is-infinite 0))))
(deftest math-sin (is (eps= (sin (/ pi 2)) 1)))
(deftest math-cos (is (eps= (cos (/ pi 2)) 0)))
(deftest math-pow (is (= (pow 2 3) 8)))
(deftest math-sqrt (is (eps= (sqrt 64) 8)))
(deftest math-abs
  (is (= (abs 5) 5))
  (is (= (abs -5) 5)))
(deftest math-ceil
  (is (= (ceil 10) 10))
  (is (= (ceil 5/2) 3))
  (is (= (ceil 5/4) 2)))
(deftest math-floor
  (is (= (floor 10) 10))
  (is (= (floor 5/2) 2))
  (is (= (floor 5/4) 1)))
(deftest math-round
  (is (= (round 10) 10))
  (is (= (round 5/2) 3))
  (is (= (round 5/4) 1)))
(deftest math-circular-vector (is (= ((circular-vector [1 2 3]) 8) 3)))
(deftest math-eps
  (is (eps= 0 0))
  (is (eps= 0 1e-20))
  (is (not (eps= 0 1e-5))))
(deftest math-plus
  (is (= (+ 1 1) 2))
  (is (not= (+ 1 2) 2)))
(deftest math-minus
  (is (= (- 10) -10))
  (is (not= (- 10) 11)))
(deftest math-multiply
  (is (= (* 2 2) 4))
  (is (not= (* 2 3) 4)))
(deftest math-divide
  (is (= (/ 10) 1/10))
  (is (not= (/ 10) 1/11)))
(deftest math-less
  (is (< 5 10))
  (is (not (< 15 10))))
(deftest math-less-eq
  (is (<= 5 10))
  (is (<= 10 10))
  (is (not (<= 15 10))))
(deftest math-greater
  (is (> 15 10))
  (is (not (> 5 10))))
(deftest math-greater-eq
  (is (>= 15 10))
  (is (>= 10 10))
  (is (not (>= 5 10))))
(deftest math-key
  (is (= (min-key identity 5 2 1 3 4) 1))
  (is (= (max-key identity 1 4 5 2 3) 5)))
