(ns pounce.math)

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
  ([x y] (< (- x) (- y)))
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
   (reduce #(clojure.core/max-key k %1 %2) (clojure.core/max-key k x y) more)))

(defn min-key
  ([k x] x)
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
   (reduce #(clojure.core/min-key k %1 %2) (clojure.core/min-key k x y) more)))

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

(defn length-squared [input] (reduce + (map #(* % %) (:data input))))
(defn length [input] (sqrt (length-squared input)))
(defn unit [input] (/ input (length input)))
(defn normal [input] 
  (if (= (size input) 2)
    (unit (column (get-cell input 1) (- (get-cell input 0))))
    nil))

(defn polynomial [& terms]
  (let [new-poly
         (into {}
           (filter #(not= (first (second %)) 0)
             (apply merge-with #(cons (+ (first %1) (first %2)) (rest %1))
                    (for [temp terms]
                      (if (number? temp)
                        {[1 1] [temp 1 1]}
                        (if (= (count temp) 3)
                          {(rest temp) temp}
                          (if (number? (first temp))
                            {[(second temp) 1] [(first temp) (second temp) 1]}
                            {temp (cons 1 temp)})))))))]
    (with-meta new-poly {:type :polynomial})))

(defmethod print-method :polynomial [x writer]
  (.write writer
    (str
      (loop [stack x result ""]
        (if (empty? stack)
          result
          (recur 
            (rest stack) 
            (str 
              result 
              (if (.isEmpty result) 
                "" 
                " + ") 
              (cond (= 1 (first (second (first stack))))
                    ""
                    (and (= -1 (first (second (first stack))))
                         (not (number? (first (first (first stack))))))
                    "-"
                    :else
                    (first (second (first stack))))
              (if (number? (first (first (first stack))))
                ""
                (first (first (first stack))))
              (if (= 1 (second (first (first stack))))
                ""
                (str "**" (second (first (first stack))))))))))))

(defmethod add [:polynomial :polynomial] [x y] 
           (apply polynomial (concat (vals x) (vals y))))
(defmethod add [:polynomial nil] [x y] 
           (apply polynomial (conj (vals x) [y 1 1])))
(defmethod add [nil :polynomial] [x y] 
           (apply polynomial (cons [x 1 1] (vals y))))

(defmethod negate :polynomial [x]
           (apply polynomial (map #(cons (- (first %)) (rest %)) (vals x))))

(defmethod multiply [:polynomial nil] [x y]
           (apply polynomial (map #(cons (* (first %) y) (rest %)) (vals x))))
(defmethod multiply [nil :polynomial] [x y]
           (apply polynomial (map #(cons (* (first %) x) (rest %)) (vals y))))

(defn constant-part [input]
  (apply polynomial (filter #(or (= (second %) 1) (= (second %) :epsilon)) (vals input))))

(defmethod less-than [:polynomial :polynomial] [x y]
           (let [max-power (apply max (concat (map second (keys (constant-part x)))
                                              (map second (keys (constant-part y)))))
                 ;q (println max-power)
                 [x-terms y-terms] (map
                                    (fn [input]
                                      ;(println "starting" input)
                                      (loop [stack (sort-by #(if (number? (second %)) 0 (last %))
                                                            (vals (constant-part input)))
                                             index (range (inc max-power))
                                             accum []]
                                        ;(println input (first stack) index)
                                        (if (empty? index)
                                          ;(do (println "return accum")
                                            accum;)
                                          (if (number? (second (first stack)))
                                            (recur (rest stack) (rest index) (conj accum (first (first stack))))
                                            (if (= (last (first stack)) (first index))
                                              (recur (rest stack) (rest index) (conj accum (first (first stack))))
                                              (recur stack (rest index) (conj accum 0)))))))
                                    [x y])]
             ;(println x-terms y-terms)
             (loop [x-stack x-terms y-stack y-terms]
               (if (empty? x-stack)
                 false
                 (cond
                  (< (first x-stack) (first y-stack))
                  true
                  (> (first x-stack) (first y-stack))
                  false
                  (= (first x-stack) (first y-stack))
                  (recur (rest x-stack) (rest y-stack)))))))

(defmethod less-than [nil :polynomial] [x y] (< x (get y [1 1] 0)))
(defmethod less-than [:polynomial nil] [x y] (< (get x [1 1] 0) y))

(defstruct lcp-step :moved-out :equations)
(defstruct linear-equation :left :right)
    
(defn move-in [system moving-in]
  (let [ratios 
         (for [equation (:equations system)]
           (if (and (get (:right equation) [moving-in 1])
                    (if (= moving-in [:z 0])
                      true
                      (> 0 (first (get (:right equation) [moving-in 1])))))
               (/ (constant-part (:right equation)) (first (get (:right equation) [moving-in 1])))
             (polynomial (if (= moving-in [:z 0]) positive-infinity negative-infinity))))
        row (if (= moving-in [:z 0])
              (apply min-key #(nth ratios %) (range (count ratios)))
              (apply max-key #(nth ratios %) (range (count ratios))))
        moving-out (first (first (keys (:left ((:equations system) row)))))
        moving-right 
          (/
            (- 
              (:left ((:equations system) row))
              (dissoc (:right ((:equations system) row)) [moving-in 1]))
            (first (get (:right ((:equations system) row)) [moving-in 1])))
        moving-left (polynomial [1 moving-in 1])
        new-equations
          (apply vector
                 (for [equation (concat (subvec (:equations system) 0 row)
                                        [(struct linear-equation moving-left moving-right)]
                                        (subvec (:equations system) (inc row)))]
              (if (contains? (:right equation) [moving-in 1])
                (struct linear-equation 
                        (:left equation)
                        (+ (dissoc (:right equation) [moving-in 1])
                           (* (first (get (:right equation) [moving-in 1])) moving-right)))
                equation)))]
    (struct lcp-step moving-out new-equations)))
    

(defn solve-lcp [M q]
  (let [raw-equations
         (struct lcp-step
                 [:w 0]    
                 (apply vector
                        (for [row (range 1 (inc (:height M)))] 
                          (struct linear-equation 
                                  (polynomial [1 [:w row]])
                                  (apply polynomial
                                         (get-cell q (dec row))
                                         [1 [:z 0]]
                                         (for [column (range 1 (inc (:width M))) :when (not= (get-cell M (dec row) (dec column)) 0)]
                                           [(get-cell M (dec row) (dec column)) [:z column]]))))))
        counting (loop [stack (map #(first (get % [1 1])) (map :right (:equations raw-equations)))
                        accum {}]
                   (if (empty? stack)
                     accum
                     (recur (rest stack) (merge-with + accum {(first stack) 1}))))
        initial-equations
        (if (= 1 (get counting (apply min (keys counting))))
          raw-equations
          (struct lcp-step
                  [:w 0]
                  (loop [stack (:equations raw-equations) accum [] index 1]
                    (if (empty? stack)
                      accum
                      (recur
                       (rest stack)
                       (conj accum (struct linear-equation
                                           (:left (first stack))
                                           (+ (:right (first stack)) (polynomial [:epsilon index]))))
                       (inc index))))))
        lcp-solution
         (loop [equations initial-equations]
           (if (= (:moved-out equations) [:z 0])
             equations
             (recur (move-in equations (if (= (first (:moved-out equations)) :w) 
                                         [:z (second (:moved-out equations))]
                                         [:w (second (:moved-out equations))])))))
        subscript-sort
          (fn [equation]
            (second (first (first (keys (:left equation))))))]
    (doseq [x (:equations initial-equations)] (println (:left x) '= (:right x)))
    {:w (for [equation (sort-by subscript-sort (:equations lcp-solution))]
          (if (= (first (first (first (keys (:left equation))))) :w)
            (first (get (:right equation) [1 1]))
            0))
     :z (for [equation (sort-by subscript-sort (:equations lcp-solution))]
          (if (= (first (first (first (keys (:left equation))))) :z)
            (first (get (:right equation) [1 1]))
            0))}))
    
  
(defn solve-convex-quadratic-problem [S A b c]
  (let [M (stack (append S (transpose A)) (append (- A) (zero (:height A))))
        q (stack (- c) b)]
    (take (:width A) (:z (solve-lcp M q)))))

(comment
  (let [s (* (transpose a) a)
        A0 (stack (- A) (A))
        b0 (stack b (c - b))
        c0 (* -2 (transpose b) A)
        K  (length-squared b)
        constraints (fn [x] (and (<= (* -1 A x) b) (<= (* A x) (- c b))))
        M (-> S (append (transpose A0)) (stack (append (- A0) (zero-matrix (rows A0)))))
        q (stack (- c0) b0)]))

(defstruct transformation-struct :type :translation :rotation)

(defn transformation [translation & rotation] 
  (struct transformation-struct 
    :transformation translation 
    (if (empty? rotation) 
      (scalar-matrix 2) 
      (if (number? (first rotation))
        (rotation-matrix (first rotation))
        (first rotation)))))

(def identity-transform 
  (transformation 
    (zero 2) 
    (scalar-matrix 2)))

(defn transform [trans point] 
  (+ (:translation trans)
    (* (:rotation trans) point)))
(defn translate [original trans] 
  (transformation (-> original :translation (+ trans)) (:rotation original)))
(defn rotate [original point angle] 
  (transformation (+ (* angle (-> original :translation (- point))) point) (-> original :rotation (* angle))))
