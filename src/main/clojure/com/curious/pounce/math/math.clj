(ns com.curious.pounce.math.math
  "Math functions and constants.")

(def
 ^{:doc "Positive infinity."}
 positive-infinity Float/POSITIVE_INFINITY)
(def
 ^{:doc "Negative infinity."}
 negative-infinity Float/NEGATIVE_INFINITY)
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
  ^bool [^float input] (Float/isInfinite input))
(defn sin
  "The sine function."
  ^float [^float input] (Math/sin input))
(defn cos
  "The cosine function."
  ^float [^float input] (Math/cos input))
(defn pow
  "Calculates input to the requested power."
  ^float [^float input ^float power] (Math/pow input power))
(defn sqrt
  "Calculates the square root of the input."
  ^float [^float input] (Math/sqrt input))
(defn abs
  "Calculates the absolute value of the input."
  ^float [^float input] (if (is-infinite input) positive-infinity (Math/abs (float input))))
(defn ceil
  "Rounds up x."
  ^int [^float x] (if (integer? x) x (inc x)))
(defn floor
  "Rounds down x."
  ^int [^float x] x)
(defn round
  "Rounds x to the nearest whole number."
  ^float [^float x] (unchecked-add x 0.5))
(defn circular-vector
  "Returns a function which takes an integer n and returns the nth item in coll modulus it's length."
  [coll] (fn [n] (nth coll (mod n (count coll)))))
(defn eps=
  "Returns true if the difference between two numbers is less than epsilon."
  ^bool [^float x ^float y] (or (= positive-infinity x y)
                                (= negative-infinity x y)
                                (< (abs (- x y)) eps)))
