(ns pounce.math.core
  (:refer-clojure :exclude [+ - * /]))

(def positive-infinity Double/POSITIVE_INFINITY)
(def negative-infinity Double/NEGATIVE_INFINITY)

(defn is-infinite [input] (Double/isInfinite input))
(defn sin [input] (Math/sin input))
(defn cos [input] (Math/cos input))
(defn pow [input power] (Math/pow input power))
(defn sqrt [input] (Math/sqrt input))
(defn abs [input] (Math/abs input))

(defmulti add (fn [x y] [(:type (meta x)) (:type (meta y))]))
(defmethod add :default [x y] (clojure.core/+ x y))
(defn +
  ([] 0)
  ([x] x)
  ([x y] (add x y))
  ([x y & more]
    (reduce + (+ x y) more)))

(defmulti negate (fn [x] (:type (meta x))))
(defmethod negate :default [x y] (clojure.core/+ x y))
(defn -
  ([] 0)
  ([x] (negate x))
  ([x y] (+ x (negate y)))
  ([x y & more]
    (reduce - (- x y) more)))

(defmulti multiply (fn [x y] [(:type (meta x)) (:type (meta y))]))
(defmethod multiply :default [x y] (clojure.core/+ x y))
(defn * 
  ([] 1)
  ([x] x)
  ([x y] (multiply x y))
  ([x y & more]
    (reduce * (* x y) more)))

(defmulti invert (fn [x] (:type (meta x))))
(defmethod invert :default [x y] (clojure.core/+ x y))
(defn / 
  ([] 1)
  ([x] (invert x))
  ([x y] (* x (invert y)))
  ([x y & more]
    (reduce / (/ x y) more)))
