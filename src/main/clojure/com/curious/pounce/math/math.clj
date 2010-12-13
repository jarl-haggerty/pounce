(ns com.curious.pounce.math.math
  "Math functions and constants."
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key]))

(def
 ^{:doc "Positive infinity."}
 positive-infinity Double/POSITIVE_INFINITY)
(def
 ^{:doc "Negative infinity."}
 negative-infinity Double/NEGATIVE_INFINITY)
(def
 ^{:doc "NaN."}
 nan Double/NaN)
(def
 ^{:doc "Pi."}
 pi Math/PI)
(def
 ^{:doc "Epsilon, the precision with which floating point numbers will be compared."}
 eps 1e-10)

(defn is-infinite
  "Determines if a number is infinite."
  [input] (Double/isInfinite input))
(defn sin
  ^{:doc "The sine function."}
  [input] (Math/sin (float input)))
(defn cos
  ^{:doc "The cosine function."}
  [input] (Math/cos input))
(defn pow
  ^{:doc "Calculates input to the requested power."}
  [input power] (Math/pow input power))
(defn sqrt
  ^{:doc "Calculates the square root of the input."}
  [input] (Math/sqrt input))
(defn abs
  ^{:doc "Calculates the absolute value of the input."}
  [input] (if (is-infinite input) positive-infinity (Math/abs (float input))))
(defn ceil
  ^{:doc "Rounds up x."}
  [x] (if (integer? x) x (inc (int x))))
(defn floor
  ^{:doc "Rounds down x."}
  [x] (int x))
(defn round
  ^{:doc "Rounds x to the nearest whole number."}
  [x] (int (clojure.core/+ x 1/2)))
(defn circular-vector
  ^{:doc "Returns a function which takes an integer n and returns the nth item in coll modulus it's length."}
  [coll] (fn [n] (nth coll (mod n (count coll)))))
(defn eps=
  ^{:doc "Returns true if the difference between two numbers is less than epsilon."}
  ([x y] (eps= x y eps))
  ([x y eps] (or (clojure.core/= positive-infinity x y)
                 (clojure.core/= negative-infinity x y)
                 (clojure.core/< (abs (clojure.core/- x y)) eps))))

(defmulti add
  "Adds x and y based on the value under type in the metadata of x and y."
  (fn [x y] [(:type (meta x)) (:type (meta y))]))
(defmethod add :default [x y] (clojure.core/+ x y))
(defn +
  "Adds the arguments."
  ([] 0)
  ([x] x)
  ([x y] (add x y))
  ([x y & more]
    (reduce + (+ x y) more)))

(defmulti negate
  "Negates x based on the value of :type in the metadata"
  (fn [x] (:type (meta x))))
(defmethod negate :default [x] (clojure.core/- x))
(defn -
   "Subtracts the remaining arguments from the first."
  ([] 0)
  ([x] (negate x))
  ([x y] (+ x (negate y)))
  ([x y & more]
    (reduce - (- x y) more)))

(defmulti multiply
  "Multiplies x and y based on the value under type in the metadata of x and y."
  (fn [x y] [(:type (meta x)) (:type (meta y))]))
(defmethod multiply :default [x y] (clojure.core/* x y))
(defn *
  "Multiplies the arguments."
  ([] 1)
  ([x] x)
  ([x y] (multiply x y))
  ([x y & more]
    (reduce * (* x y) more)))

(defmulti invert
  "Inverts x based on the value of type in the metadata."
  (fn [x] (:type (meta x))))
(defmethod invert :default [x] (clojure.core// x))
(defn /
  "Divides the first argument by the remaining arguments."
  ([] 1)
  ([x] (invert x))
  ([x y] (* x (invert y)))
  ([x y & more]
    (reduce / (/ x y) more)))

(defmulti less-than
  "Returns true if x is less than y based on the value under type in the metadata of x and y."
  (fn [x y] [(:type (meta x)) (:type (meta y))]))
(defmethod less-than :default [x y] (clojure.core/< x y))
(defn <
  "Determines if the arguments are in strictly ascending order."
  ([] true)
  ([x] true)
  ([x y] (less-than x y))
  ([x y & more]
     (reduce < (< x y) more)))

(defmulti equal
  "Returns true if x is equal to y based on the value under type in the metadata of x and y."
  (fn [x y]
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
  "Returns true is the arguments are equal."
  ([x] true)
  ([x y] (equal x y))
  ([x y & more]
   (if (= x y)
     (if (next more)
       (recur y (first more) (next more))
       (= y (first more)))
     false)))
(defn seq=
  "Compares sequences element by element and returns true if the contents are equal."
  ([x] true)
  ([x y] (and (= (count x) (count y)) (every? identity (map #(= %1 %2) x y))))
  ([x y & more]
     (if (seq= x y)
       (if (next more)
         (recur y (first more) (next more))
         (seq= y (first more)))
       false)))

(defn not=
  "The complement of =."
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more]
     (not (apply = x y more))))
(defn not-seq=
  "The complement of seq=."
  ([x] false)
  ([x y] (not (seq= x y)))
  ([x y & more]
     (not (apply seq= x y more))))

(defn <=
  "Determines if the arguments are in ascending order."
  ([] true)
  ([x] true)
  ([x y] (or (= x y) (< x y)))
  ([x y & more]
      (reduce <= (<= x y) more)))

(defn >
  "Determines if the arguments are in strictly descending order."
  ([] true)
  ([x] true)
  ([x y] (< y x))
  ([x y & more]
      (reduce > (> x y) more)))

(defn >=
  "Determines if the arguments are in descending order."
  ([] true)
  ([x] true)
  ([x y] (or (= x y) (> x y)))
  ([x y & more]
     (reduce >= (>= x y) more)))

(defn max-key
  "Returns the argument for which (k z) is maximal."
  ([k x] x)
  ([k x y] (if (> (k x) (k y)) x y))
  ([k x y & more]
   (reduce #(max-key k %1 %2) (max-key k x y) more)))

(defn min-key
  "Returns the argument for which (k z) is minimal."
  ([k x] x)
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
   (reduce #(min-key k %1 %2) (min-key k x y) more)))
