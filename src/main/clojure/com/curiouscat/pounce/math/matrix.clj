(ns com.curiouscat.pounce.math.matrix
  (:refer-clojure :exclude [+ - * / < <= > >= max-key min-key])
  (:use com.curiouscat.pounce.math.math clojure.test))

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
  ([height width] (matrix (repeat (* height width) 0) height width)))

(defn scalar
  ([size scale] (matrix (map #(if (= (int (/ % size)) (mod % size)) scale 0) (range (* size size))) size size))
  ([size] (scalar size 1)))

(defn component
  ([size where scale] (matrix (map #(if (= % where) scale 0) (range size)) size 1))
  ([size where] (component size where 1)))

(defn get-cell 
  ([M position] (get-cell M position 0))
  ([M row column] ((:data M) (+ row (* (:height M) column)))))

(defn set-cell
  ([M position value] (set-cell M position 0 value))
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

(defn row-minor [M row]
  (if (= (:height M) 1)
    nil
    (let [matrix-rows (apply vector (rows M))]
      (matrix (apply interleave (concat (subvec matrix-rows 0 row) (subvec matrix-rows (inc row)))) (dec (:height M)) (:width M)))))

(defn column-minor [M col]
  (if (= (:width M) 1)
    nil
    (let [matrix-columns (apply vector (columns M))]
      (matrix (apply concat (concat (subvec matrix-columns 0 col) (subvec matrix-columns (inc col)))) (:height M) (dec (:width M))))))

(defn minor [M row column]
  (if (or (= 1 (:height M)) (= 1 (:width M)))
    nil
    (-> M (row-minor row) (column-minor column))))

(defn size [M] 
  (count (:data M)))
(comment
  (defmethod print-method :matrix [M writer]
             (let [largest-number (reduce max (map #(-> % str count) (:data M)))]
               (doseq [row (range (:height M)) column (range (:width M))]
                 (if (and (= (dec (:width M)) column) (< row (dec (:height M))))
                   (print-method (format (str "%" largest-number "s\n") (str (get-cell M row column))) writer)
                   (print-method (format (str "%" largest-number "s ") (str (get-cell M row column))) writer)
                   )))))

(defmethod add [nil :matrix] [x y] (matrix (map #(+ x %) (:data y)) (:height y) (:width y)))
(defmethod add [:matrix nil] [x y] (matrix (map #(+ % y) (:data x)) (:height x) (:width x)))
(defmethod add [:matrix :matrix] [x y] (matrix (map + (:data x) (:data y)) (:height x) (:width x)))

(defmethod negate :matrix [x] (matrix (map - (:data x)) (:height x) (:width x)))

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

(defn determinant [M]
  (if (= (:height M) (:width M))
    (condp = [(:height M) (:width M)]
        [1 1] (get-cell M 0 0)
        [2 2] (- (* (get-cell M 0 0) (get-cell M 1 1)) (* (get-cell M 0 1) (get-cell M 1 0)))
        (reduce +
                (map #(* (determinant (minor M 0 %)) (pow -1 (+ 2 %))) (range (:width M)))))
    0))

(defn cross [a b] (determinant (append a b)))
(defn dot [a b] (get-cell (* (transpose a) b) 0))
(defn length-squared [input] (dot input input))
(defn length [input] (sqrt (length-squared input)))
(defn unit [input] (/ input (length input)))
(defn normal [input] 
    (unit (matrix (y input) (- (x input)))))

(defn rotation [theta] (matrix [(cos theta) (sin theta) (- (sin theta)) (cos theta)] 2 2))
(defn transform
  ([x y angle] (transform (matrix [x y] 2 1) angle))
  ([displacement angle] (with-meta  {:translation displacement :rotation (rotation angle)} {:type :transform})))
(def identity-transform (transform 0 0 0))

(deftest cross-test (is (= (cross (matrix 2 0) (matrix 0 3)) 6)))
(deftest dot-test (is (= (dot (matrix 1 2) (matrix 3 4)) 11)))
(deftest unit-test (is (= (unit (matrix 0 22)) (matrix 0 1))))
(deftest normal-1 (is (every? identity (map eps= (:data (normal (matrix 0 3))) (:data (matrix 1 0))))))
(deftest normal-2 (is (every? identity (map eps= (:data (normal (matrix 5 0))) (:data (matrix 0 -1))))))
(deftest rotation-test (is (map eps= (:data (rotation (/ pi 2))) [0 1 -1 0])))
(deftest transform-1-rotation
  (is (map eps= (:data (:rotation (transform 1 2 (/ pi 2)))) [0 1 -1 0])))
(deftest transform-1-translation
  (is (map eps= (:data (:translation (transform 1 2 (/ pi 2)))) [1 2])))
(deftest transform-2-rotation
  (is (map eps= (:data (:rotation (transform (matrix 1 2) (/ pi 2)))) [0 1 -1 0])))
(deftest transform-2-translation
  (is (map eps= (:data (:translation (transform (matrix 1 2) (/ pi 2)))) [1 2])))
(deftest matrix-1-arg-number (is (= (matrix 1) {:data [1] :height 1 :width 1})))
(deftest matrix-1-arg-seq (is (= (matrix '(1 2 3)) {:data [1 2 3] :height 3 :width 1})))
(deftest matrix-1-arg-seq-in-seq (is (= (matrix '((1 2 3) (4 5 6) (7 8 9))) {:data [1 2 3 4 5 6 7 8 9] :height 3 :width 3})))
(deftest matrix-1-arg-vector (is (= (matrix [1 2 3]) {:data [1 2 3] :height 3 :width 1})))
(deftest matrix-1-arg-vector-in-vector (is (= (matrix [[1 2 3] [4 5 6] [7 8 9]]) {:data [1 2 3 4 5 6 7 8 9] :height 3 :width 3})))
(deftest matrix-2-arg (is (= (matrix 1 2) {:data [1 2] :height 2 :width 1})))
(deftest matrix-3-arg-number (is (= (matrix 1 2 3) {:data [1 2 3] :height 3 :width 1})))
(deftest matrix-3-arg-vector (is (= (matrix [1 2 3 4 5 6 7 8 9] 3 3) {:data [1 2 3 4 5 6 7 8 9] :height 3 :width 3})))
(deftest matrix-3-arg-seq (is (= (matrix '(1 2 3 4 5 6 7 8 9) 3 3) {:data [1 2 3 4 5 6 7 8 9] :height 3 :width 3})))
(deftest matrix-n-arg (is (= (matrix 1 2 3 4 5 6 7 8 9) {:data [1 2 3 4 5 6 7 8 9] :height 9 :width 1})))
(deftest stack-matrix (is (= (stack (matrix 1 2 3) (matrix 4 5 6) (matrix 7 8 9)) {:data [1 2 3 4 5 6 7 8 9] :height 9 :width 1})))
(deftest stack-matrix-2
  (is (= (stack (matrix [1 2 3] 1 3) (matrix [4 5 6] 1 3) (matrix [7 8 9] 1 3)) {:data [1 4 7 2 5 8 3 6 9] :height 3 :width 3})))
(deftest append-matrix (is (= (append (matrix 1 2 3) (matrix 4 5 6) (matrix 7 8 9)) {:data [1 2 3 4 5 6 7 8 9] :height 3 :width 3})))
(deftest append-matrix
  (is (= (append (matrix [1 2 3] 1 3) (matrix [4 5 6] 1 3) (matrix [7 8 9] 1 3)) {:data [1 2 3 4 5 6 7 8 9] :height 1 :width 9})))
(deftest zero-1 (is (= (zero 3) {:data [0 0 0] :height 3 :width 1})))
(deftest zero-2 (is (= (zero 3 3) {:data [0 0 0 0 0 0 0 0 0] :height 3 :width 3})))
(deftest scalar-1 (is (= (scalar 3) {:data [1 0 0 0 1 0 0 0 1] :height 3 :width 3})))
(deftest scalar-2 (is (= (scalar 3 5) {:data [5 0 0 0 5 0 0 0 5] :height 3 :width 3})))
(deftest component-1-x (is (= (component 3 0) {:data [1 0 0] :height 3 :width 1})))
(deftest component-1-y (is (= (component 3 1) {:data [0 1 0] :height 3 :width 1})))
(deftest component-1-z (is (= (component 3 2) {:data [0 0 1] :height 3 :width 1})))
(deftest component-2-x (is (= (component 3 0 3) {:data [3 0 0] :height 3 :width 1})))
(deftest component-2-y (is (= (component 3 1 3) {:data [0 3 0] :height 3 :width 1})))
(deftest component-2-z (is (= (component 3 2 3) {:data [0 0 3] :height 3 :width 1})))
(deftest get-cell-1d-1 (is (= (get-cell (matrix 1 2 3) 0) 1)))
(deftest get-cell-1d-2 (is (= (get-cell (matrix 1 2 3) 1) 2)))
(deftest get-cell-1d-3 (is (= (get-cell (matrix 1 2 3) 2) 3)))
(deftest get-cell-2d-1 (is (= (get-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 0 0) 1)))
(deftest get-cell-2d-2 (is (= (get-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 1 0) 2)))
(deftest get-cell-2d-3 (is (= (get-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 2 0) 3)))
(deftest get-cell-2d-4 (is (= (get-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 0 1) 4)))
(deftest get-cell-2d-5 (is (= (get-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 1 1) 5)))
(deftest get-cell-2d-6 (is (= (get-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 2 1) 6)))
(deftest get-cell-2d-7 (is (= (get-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 0 2) 7)))
(deftest get-cell-2d-8 (is (= (get-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 1 2) 8)))
(deftest get-cell-2d-9 (is (= (get-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 2 2) 9)))
(deftest set-cell-1d-1 (is (= (set-cell (matrix 1 2 3) 0 0) {:data [0 2 3] :height 3 :width 1})))
(deftest set-cell-1d-2 (is (= (set-cell (matrix 1 2 3) 1 0) {:data [1 0 3] :height 3 :width 1})))
(deftest set-cell-1d-3 (is (= (set-cell (matrix 1 2 3) 2 0) {:data [1 2 0] :height 3 :width 1})))
(deftest set-cell-2d-1 (is (= (set-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 0 0 0) {:data [0 2 3 4 5 6 7 8 9] :height 3 :width 3})))
(deftest set-cell-2d-2 (is (= (set-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 1 0 0) {:data [1 0 3 4 5 6 7 8 9] :height 3 :width 3})))
(deftest set-cell-2d-3 (is (= (set-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 2 0 0) {:data [1 2 0 4 5 6 7 8 9] :height 3 :width 3})))
(deftest set-cell-2d-4 (is (= (set-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 0 1 0) {:data [1 2 3 0 5 6 7 8 9] :height 3 :width 3})))
(deftest set-cell-2d-5 (is (= (set-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 1 1 0) {:data [1 2 3 4 0 6 7 8 9] :height 3 :width 3})))
(deftest set-cell-2d-6 (is (= (set-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 2 1 0) {:data [1 2 3 4 5 0 7 8 9] :height 3 :width 3})))
(deftest set-cell-2d-7 (is (= (set-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 0 2 0) {:data [1 2 3 4 5 6 0 8 9] :height 3 :width 3})))
(deftest set-cell-2d-8 (is (= (set-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 1 2 0) {:data [1 2 3 4 5 6 7 0 9] :height 3 :width 3})))
(deftest set-cell-2d-9 (is (= (set-cell (matrix [1 2 3 4 5 6 7 8 9] 3 3) 2 2 0) {:data [1 2 3 4 5 6 7 8 0] :height 3 :width 3})))
(deftest x-test (is (= (x (matrix 1 2)) 1)))
(deftest y-test (is (= (y (matrix 1 2)) 2)))
(deftest columns-test (is (= (columns (matrix [1 2 3 4 5 6 7 8 9] 3 3)) [[1 2 3] [4 5 6] [7 8 9]])))
(deftest rows-test (is (= (rows (matrix [1 2 3 4 5 6 7 8 9] 3 3)) [[1 4 7] [2 5 8] [3 6 9]])))
(deftest transpose-1 (is (= (transpose (matrix 1 2 3)) {:data [1 2 3] :height 1 :width 3})))
(deftest transpose-2 (is (= (transpose (matrix [1 2 3 4 5 6 7 8 9] 3 3)) {:data [1 4 7 2 5 8 3 6 9] :height 3 :width 3})))
(deftest row-minor-1 (is (= (row-minor (matrix [1 2 3] 1 3) 1) nil)))
(deftest row-minor-2 (is (= (row-minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 0) {:data [2 3 5 6 8 9] :height 2 :width 3})))
(deftest row-minor-3 (is (= (row-minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 1) {:data [1 3 4 6 7 9] :height 2 :width 3})))
(deftest row-minor-4 (is (= (row-minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 2) {:data [1 2 4 5 7 8] :height 2 :width 3})))
(deftest column-minor-1 (is (= (column-minor (matrix [1 2 3] 3 1) 1) nil)))
(deftest column-minor-2 (is (= (column-minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 0) {:data [4 5 6 7 8 9] :height 3 :width 2})))
(deftest column-minor-3 (is (= (column-minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 1) {:data [1 2 3 7 8 9] :height 3 :width 2})))
(deftest column-minor-4 (is (= (column-minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 2) {:data [1 2 3 4 5 6] :height 3 :width 2})))
(deftest minor-1 (is (= (minor (matrix [1 2 3] 1 3) 1 1) nil)))
(deftest minor-2 (is (= (minor (matrix [1 2 3] 3 1) 1 1) nil)))
(deftest minor-3 (is (= (minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 0 0) {:data [5 6 8 9] :height 2 :width 2})))
(deftest minor-4 (is (= (minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 1 0) {:data [4 6 7 9] :height 2 :width 2})))
(deftest minor-5 (is (= (minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 2 0) {:data [4 5 7 8] :height 2 :width 2})))
(deftest minor-6 (is (= (minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 0 1) {:data [2 3 8 9] :height 2 :width 2})))
(deftest minor-7 (is (= (minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 1 1) {:data [1 3 7 9] :height 2 :width 2})))
(deftest minor-8 (is (= (minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 2 1) {:data [1 2 7 8] :height 2 :width 2})))
(deftest minor-9 (is (= (minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 0 2) {:data [2 3 5 6] :height 2 :width 2})))
(deftest minor-10 (is (= (minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 1 2) {:data [1 3 4 6] :height 2 :width 2})))
(deftest minor-11 (is (= (minor (matrix [1 2 3 4 5 6 7 8 9] 3 3) 2 2) {:data [1 2 4 5] :height 2 :width 2})))
(deftest size-1 (is (= (size (matrix [1 2 3] 3 1)) 3)))
(deftest size-2 (is (= (size (matrix [1 2 3] 1 3)) 3)))
(deftest size-3 (is (= (size (matrix [1 2 3 4 5 6 7 8 9] 3 3)) 9)))
(deftest add-1
  (is (= (+ (matrix [1 2 3 4 5 6 7 8 9] 3 3) (matrix [1 2 3 4 5 6 7 8 9] 3 3)) {:data [2 4 6 8 10 12 14 16 18] :height 3 :width 3})))
(deftest add-2 (is (= (+ (matrix [1 2 3 4 5 6 7 8 9] 3 3) 2) {:data [3 4 5 6 7 8 9 10 11] :height 3 :width 3})))
(deftest add-3 (is (= (+ 2 (matrix [1 2 3 4 5 6 7 8 9] 3 3)) {:data [3 4 5 6 7 8 9 10 11] :height 3 :width 3})))
(deftest negate-test (is (= (- (matrix [1 2 3 4 5 6 7 8 9] 3 3)) {:data [-1 -2 -3 -4 -5 -6 -7 -8 -9] :height 3 :width 3})))
(deftest multiply-1
  (is
   (=
    (* (matrix [1 2 3 4 5 6 7 8 9] 3 3) (matrix [1 2 3 4 5 6 7 8 9] 3 3))
    {:data [30 36 42 66 81 96 102 126 150] :height 3 :width 3})))
(deftest multiply-2 (is (= (* (matrix [1 2 3 4 5 6 7 8 9] 3 3) 2) {:data [2 4 6 8 10 12 14 16 18] :height 3 :width 3})))
(deftest multiply-3 (is (= (* 2 (matrix [1 2 3 4 5 6 7 8 9] 3 3)) {:data [2 4 6 8 10 12 14 16 18] :height 3 :width 3})))
(deftest determinant-1 (is (= (determinant (matrix 2)) 2)))
(deftest determinant-2 (is (= (determinant (matrix [1 2 3 4] 2 2)) -2)))
(deftest determinant-3 (is (= (determinant (matrix [1 0 0 0 2 0 0 0 3] 3 3)) 6)))
(deftest determinant-4 (is (= (determinant (matrix 1 2)) 0)))
