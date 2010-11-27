(ns pounce.math.matrix
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
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

(defn matrix? [M] (= (:type (meta M)) :matrix))
(defn mat [M] (if (matrix? M) M (matrix M)))

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
(defmethod add [:transform :matrix] [x y] (assoc :translation (+ (:translation x) y)))

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
           (matrix (take 2 (:data (* (stack (append (:rotation x) (:translation x)) (matrix [0 0 1] 1 3))
                                            (stack y (matrix 1)))))))

(defmethod equal [:matrix :matrix] [x y] (and (= (:width x) (:width y))
                                              (= (:height x) (:height y))
                                              (every? identity (map eps= (:data x) (:data y)))))
;(defmethod equal [:matrix :float] [x y] (every? identity (map eps= (:data x) (:data y))))
;(defmethod equal [:matrix :integer] [x y] (every? identity (map eps= (:data x) (:data y))))
;(defmethod equal [:matrix :matrix] [x y] (every? identity (map eps= (:data x) (:data y))))
;(defmethod equal [:matrix :matrix] [x y] (every? identity (map eps= (:data x) (:data y))))

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
  ([displacement angle] (with-meta  {:translation (mat displacement) :rotation (if (matrix? angle) angle (rotation angle))} {:type :transform})))
(def identity-transform (transform 0 0 0))
(defn transform? [T] (= (:type (meta T)) :transform))

(defmethod equal [:transform :transform] [x y] (and (= (:translation x) (:translation y)) (= (:rotation x) (:rotation y))))

(defn transform-about [trans center & points]
  (let [result (for [point points]
                  (+ (* trans
                        (- point center))
                     center))]
    (if (= (count result) 1)
      (first result)
      result)))
