(ns pounce.math.matrix
  (:use pounce.math.operations))

(defstruct matrix-struct :data :height :width)

(defn matrix 
  ([data height width] 
    (with-meta (struct matrix-struct (if (vector? data) data (apply vector data)) height width) {:type :matrix}))
  ([data]
     (matrix data (count data) 1)))

(defn column [& data]
  (matrix data (count data) 1))

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
           (matrix (take 2 (multiply (stack (append (:rotation) (:translation x)) (matrix [0 0 1] 1 2))
                                     (stack y (matrix [1] 1 1))))
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
  (apply +
    (for [permutation (permutations (range (:width M)))]
      (apply * (signature permutation) 
        (for [i (range (:width M))]
          (get-cell M i (permutation i)))))))

(defn cofactor-matrix [M]
  (matrix 
    (for [row (range (:height M)) column (range (:width M))]
      (* 
        (if (even? (+ column row)) 1 -1)
        (determinant (minor M row column))))
    (:height M)
    (:width M)))

(defmethod invert :matrix [x] (/ (transpose (cofactor-matrix x)) (determinant x)))

(defn X [a b] 
  (- (* (get-cell a 0) (get-cell b 1)) (* (get-cell b 0) (get-cell a 1))))
(defn dot [a b] (- (+ (map * (:data a) (:data b))) 1))
(defn length-squared [input] (dot input input))
(defn length [input] (sqrt (length-squared input)))
(defn unit [input] (/ input (length input)))
(defn normal [input] 
  (if (= (size input) 2)
    (unit (column (get-cell input 1) (- (get-cell input 0))))
    nil))

(defn x [M] (get-cell M 0))
(defn y [M] (get-cell M 1))

(defn rotation [theta] (matrix [(cos theta) (sin theta) (- (sin theta)) (cos theta)] 2 2))
(defstruct transform-struct :translation :rotation)
(defn transform
  ([x y angle] (transform (matrix [x y] 2 1) angle))
  ([displacement angle] (with-meta (struct transform-struct displacement (rotation theta)) {:type :transform})))
