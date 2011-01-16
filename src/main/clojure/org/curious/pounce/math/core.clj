(ns org.curious.pounce.math.core
  "Math functions and constants.")

(def
 ^{:doc "Positive infinity"}
 positive-infinity Float/POSITIVE_INFINITY)
(def
 ^{:doc "Negative infinity"}
 negative-infinity Float/NEGATIVE_INFINITY)
(def
 ^{:doc "NaN"}
 nan Float/NaN)
(def
 ^{:doc "Pi"}
 pi (float Math/PI))
(def
 ^{:doc "Epsilon, the precision with which floating point numbers will be compared."}
 eps (float 1e-7))

(defn is-infinite
  "Determines if a number is infinite."
  [input] (Float/isInfinite input))
(defn sin
  "The sine function."
  [input] (float (Math/sin input)))
(defn cos
  "The cosine function."
  [input] (float (Math/cos input)))
(defn pow
  "Calculates input to the requested power."
  [input power] (float (Math/pow input power)))
(defn sqrt
  "Calculates the square root of the input."
  [input] (float (Math/sqrt input)))
(defn abs
  "Calculates the absolute value of the input."
  [input] (if (is-infinite input) positive-infinity (float (Math/abs (float input)))))
(defn ceil
  "Rounds up x."
  [x] (if (integer? x) x (unchecked-inc (int x))))
(defn floor
  "Rounds down x."
  [x] (int x))
(defn round
  "Rounds x to the nearest whole number."
  [x] (int (+ x 0.5)))
(defn circular-indexer
  "Returns a function which takes an integer n and returns the nth item in coll modulus it's length."
  [coll] (fn [n] (coll (mod n (count coll)))))
(defn eps=
  "Returns true if the difference between two numbers is less than epsilon."
  ([x y] (eps= x y eps))
  ([x y eps] (or (= positive-infinity x y)
                 (= negative-infinity x y)
                 (< (abs (- x y)) eps))))
(defn eps<
  "Returns true if the difference between two numbers is less than epsilon."
  [x y] (and (not (eps= x y))
             (< x y)))
(defn eps<=
  "Returns true if the difference between two numbers is less than epsilon."
  [x y] (or (eps< x y) (eps= x y)))
(defn eps>
  "Returns true if the difference between two numbers is less than epsilon."
  [x y] (not (eps<= x y)))
(defn eps>=
  "Returns true if the difference between two numbers is less than epsilon."
  [x y] (not (eps< x y)))
