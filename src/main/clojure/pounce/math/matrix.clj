(ns pounce.math.matrix
  (:refer-clojure :exclude [+ - * / < <= > >= max-key min-key])
  (:use pounce.math.math))

(defn matrix
  ([data]
     (if (sequential? data)
       (if (sequential? (first data))
         (matrix (apply concat data) (count (first data)) (count data))
         (matrix data (count data) 1))
       (matrix [data] 1 1)))
  ([one two] (matrix [one two] 2 1))
  ([data height width]
     (if (sequential? data)
       (with-meta {:data (if (vector? data) data (vec data)) :height height :width width} {:type :matrix})
       (matrix [data height width] 3 1)))
  ([one two three & data]
     (matrix (concat [one two three] data) (+ (count data) 3) 1)))

(defn zero
  ([height] (zero height 1))
  ([height width] (matrix (map #(do % 0) (range (* height width))) height width)))

(defn scalar-matrix
  ([size scale] (matrix (map #(if (= (/ % size) (mod % size)) scale 0) (range (* size size))) size size))
  ([size] (scalar-matrix size 1)))

(defn component
  ([length where scale] (matrix (map #(if (= % where) scale 0) (range length)) length 1))
  ([length where] (component where 1)))

(defn get-cell 
  ([M position] (if (= (:height M) 1) (get-cell M 0 position) (get-cell M position 0)))
  ([M row column] ((:data M) (+ row (* (:height M) column)))))

(defn set-cell
  ([M position value] (if (= (:height M) 1) (set-cell M 0 position value) (set-cell M position 0 value)))
  ([M row column value] (matrix (assoc (:data M) (+ row (* (:height M) column)) value) (:height M) (:width M))))

(defn x [M] (get-cell M 0))
(defn y [M] (get-cell M 1))

(defn columns [M] 
  (for [column (range (:width M))] 
    (subvec (:data M) (* column (:height M)) (* (inc column) (:height M)))))

(defn rows [M] 
  (for [row (range (:height M))] 
    (apply vector (for [x (range row (count (:data M)) (:height M))] 
                    ((:data M) x)))))
(defn stack
  ([M] M)
  ([M N] 
    (if (= (:width M) (:width N))
      (matrix (flatten (interleave (columns M) (columns N))) (+ (:height M) (:height N)) (:width M))
      nil))
  ([M N & more] 
     (reduce stack (stack M N) more)))

(defn append 
  ([M] M)
  ([M N] 
    (if (= (:height M) (:height N))
      (matrix (flatten (concat (columns M) (columns N))) (:height M) (+ (:width M) (:width N)))
      nil))
  ([M N & more] 
     (reduce append (append M N) more)))

(defn transpose [M] 
  (matrix (flatten (rows M)) (:width M) (:height M)))

(defn rotation-matrix
  ([angle] (matrix [(cos angle) (- (sin angle)) (sin angle) (cos angle)] 2 2)))

(defn row-minor [M row]
  (let [matrix-rows (apply vector (rows M))]
    (matrix (apply interleave (concat (subvec matrix-rows 0 row) (subvec matrix-rows (inc row)))) (dec (:height M)) (:width M))))

(defn column-minor [M col]
  (let [matrix-columns (apply vector (columns M))]
    (matrix (apply concat (concat (subvec matrix-columns 0 col) (subvec matrix-columns (inc col)))) (:height M) (dec (:width M)))))

(defn minor [M row column]
  (-> M (row-minor row) (column-minor column)))

(defn size [M] 
  (count (:data M)))

(defmethod print-method :matrix [M writer]
  (let [largest-number (reduce max (map #(-> % str count) (:data M)))]
      (doseq [row (range (:height M)) column (range (:width M))]
        (if (and (= (dec (:width M)) column) (< row (dec (:height M))))
          (print-method (format (str "%" largest-number "s\n") (str (get-cell M row column))) writer)
          (print-method (format (str "%" largest-number "s ") (str (get-cell M row column))) writer)
          ))))

(defmethod add [nil :matrix] [x y] (matrix (map #(+ x %) (:data y)) (:height y) (:width y)))
(defmethod add [:matrix nil] [x y] (matrix (map #(+ % y) (:data x)) (:height x) (:width x)))
(defmethod add [:matrix :matrix] [x y] (matrix (map + (:data x) (:data y)) (:height x) (:width x)))

(defmethod negate :matrix [x] (println x) (matrix (map - (:data x)) (:height x) (:width x)))

(defmethod multiply [nil :matrix] [x y] (matrix (map #(* x %) (:data y)) (:height y) (:width y)))
(defmethod multiply [:matrix nil] [x y] (matrix (map #(* % y) (:data x)) (:height x) (:width x)))
(defmethod multiply [:matrix :matrix] [x y] 
  (matrix 
    (for [column (columns y) row (rows x)]
      (reduce + (map * row column)))
    (:height x)
    (:width y)))
(defmethod multiply [:transform :matrix] [x y]
           (matrix (take 2 (:data (* (stack (append (:rotation) (:translation x)) (matrix [0 0 1] 1 2))
                                            (stack y (matrix [1] 1 1)))))
                   2 1))

(defn signature [permutation] 
  (let [inversions 
         (for [x permutation]
           (for [y permutation]
             (if (> x y) 1 0)))]
    (if (even? (apply + inversions)) 1 -1)))

(defn permutations [input]
  (loop [current (map list input)]
    (if (= (count (first current)) (count input))
      current
      (recur
        (for [c current i (reduce disj (set input) c)]
          (cons i c))))))

(defn determinant [M]
  (if (= (:height M) (:width M))
    (condp = [(:height M) (:width M)]
        [1 1] (get-cell M 0 0)
        [2 2] (- (* (get-cell M 0 0) (get-cell M 1 1)) (* (get-cell M 0 1) (get-cell M 1 0)))
        (reduce +
                (map #(* (pow -1 (+ 2 %)))
                     (map #(determinant (minor M 0 %)) (range (:width M))))))
    0))

;(defmethod invert :matrix [x] (/ (transpose (cofactor-matrix x)) (determinant x)))

(defn cross [a b] (determinant (append a b)))
(defn dot [a b] (* (transpose a) b))
(defn length-squared [input] (dot input input))
(defn length [input] (sqrt (length-squared input)))
(defn unit [input] (/ input (length input)))
(defn normal [input] 
  (if (= (size input) 2)
    (unit (matrix (get-cell input 1) (- (get-cell input 0))))
    nil))

(defn rotation [theta] (matrix [(cos theta) (sin theta) (- (sin theta)) (cos theta)] 2 2))
(defn transform
  ([x y angle] (transform (matrix [x y] 2 1) angle))
  ([displacement angle] (with-meta  {:translation displacement :rotation (rotation angle)} {:type :transform})))
(def identity-transform (transform 0 0 0))
