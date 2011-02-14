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

(ns org.curious.pounce.math.core
  "Math functions and constants.")

(def primitive Double/TYPE)
(def
 ^{:doc "Positive infinity"}
 positive-infinity Double/POSITIVE_INFINITY)
(def
 ^{:doc "Negative infinity"}
 negative-infinity Double/NEGATIVE_INFINITY)
(def
 ^{:doc "NaN"}
 nan Double/NaN)
(def
 ^{:doc "Pi"}
 pi Math/PI)
(def
 ^{:doc "Epsilon, the precision with which floating point numbers will be compared."}
 eps 1e-10)

(defn is-infinite
  "Determines if a number is infinite."
  [input] (Double/isInfinite input))
(defn sin
  "The sine function."
  [input] (Math/sin input))
(defn cos
  "The cosine function."
  [input] (Math/cos input))
(defn pow
  "Calculates input to the requested power."
  [input power] (Math/pow input power))
(defn sqrt
  "Calculates the square root of the input."
  [input] (Math/sqrt input))
(defn abs
  "Calculates the absolute value of the input."
  [input] (if (is-infinite input) positive-infinity (Math/abs input)))
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
<<<<<<< HEAD
  ([x y] (= x y) (eps= x y eps))
=======
  ([x y] (eps= x y eps))
>>>>>>> 719c6af97bac59532bbe0c5eb1be941b74ca31d6
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
(defn map-eps= [this that] (let [this-keys (if (map? this)
					     (-> this keys set)
					     (-> this count range))
                                 that-keys (if (map? that)
					     (-> that keys set)
					     (-> that count range))
                                 reduce-function (fn [x y]
                                                   (and x (cond
                                                           (number? (get this y)) (eps= (get this y) (get that y))
                                                           (associative? (get this y)) (map-eps= (get this y) (get that y))
                                                           :else (= (get this y) (get that y)))))]
                             (and (= this-keys that-keys)
                                  (reduce reduce-function true this-keys))))
