(ns pounce.math
  (:import 
    java.lang.Math 
    java.io.Writer)
  (:use clojure.set)
  (:refer-clojure :exclude [+ - * /]))

(def positiveInfinity Double/POSITIVE_INFINITY)
(def negativeInfinity Double/NEGATIVE_INFINITY)
(def positive-infinity Double/POSITIVE_INFINITY)
(def negative-infinity Double/NEGATIVE_INFINITY)
(def epsilon 'epsilon)

(defn isInfinite [input] (Double/isInfinite input))
(defn is-infinite [input] (Double/isInfinite input))
(defn sin [input] (Math/sin input))
(defn cos [input] (Math/cos input))
(defn pow [input power] (Math/pow input power))
(defn sqrt [input] (Math/sqrt input))
(defn abs [input] (Math/abs input))

(defstruct term-struct :coefficient :variables)
(defn term 
  [head & variable-powers]
    (cond 
      (empty? variable-powers)
      (if (number? head)
        (with-meta (struct term-struct head {}) {:type :term})
        (with-meta (struct term-struct 1 {head 1}) {:type :term}))
      (= (count variable-powers) 1)
      (let [one head two (first variable-powers)]
        (cond 
          (and (number? one) (number? two))
          (with-meta (struct term-struct (pow one two) {}) {:type :term})
          (number? one)
          (with-meta (struct term-struct one {two 1}) {:type :term})
          (number? two)
          (with-meta (struct term-struct 1 {one two}) {:type :term})))
      true
      (if (number? head)
        (with-meta 
          (struct term-struct 
            head 
            (apply merge-with clojure.core/+ 
              (for [temp (partition 2 variable-powers)] 
                {(first temp) (second temp)}))) 
          {:type :term})
        (with-meta 
          (struct term-struct 
            1 
            (apply merge-with clojure.core/+
              (for [temp (partition 2 (cons head variable-powers))] 
                {(first temp) (second temp)}))) 
          {:type :term}))))
(defn add-terms [x y]
  (if (= (:variables x) (:variables y))
    (assoc x 
      :coefficient 
      (clojure.core/+ (:coefficient x) (:coefficient y)))
    (throw (Exception. "Can't add terms"))))
(defn multiply-terms [x y]
  (apply term (clojure.core/* (:coefficient x) (:coefficient y))
    (interleave (keys (:variables x)) (vals (:variables x)) (keys (:variables y)) (vals (:variables y)))))
(defn negate-term [x]
  (assoc x :coefficient (clojure.core/- (:coefficient x))))
(defn polynomial [& terms]
  (let [new-poly
         (into {}
           (filter #(not= (:coefficient (second %)) 0)
             (apply merge-with add-terms
               (for [temp terms]
                 {(:variables temp) temp}))))]
    (with-meta new-poly {:type :polynomial})))

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
  ([height width] (matrix (map #(do % 0) (range (clojure.core/* height width))) height width)))

(defn scalar-matrix
  ([size scale] (matrix (map #(if (= (clojure.core// % size) (mod % size)) scale 0) (range (clojure.core/* size size))) size size))
  ([size] (scalar-matrix size 1)))

(defn component
  ([length where scale] (matrix (map #(if (= % where) scale 0) (range length)) length 1))
  ([length where] (component where 1)))

(defn get-cell 
  ([M position] (if (= (:height M) 1) (get-cell M 0 position) (get-cell M position 0)))
  ([M row column] ((:data M) (clojure.core/+ row (clojure.core/* (:height M) column)))))

(defn columns [M] 
  (for [column (range (:width M))] 
    (subvec (:data M) (clojure.core/* column (:height M)) (clojure.core/* (inc column) (:height M)))))

(defn rows [M] 
  (for [row (range (:height M))] 
    (apply vector (for [x (range row (count (:data M)) (:height M))] 
                    ((:data M) x)))))
(defn stack
  ([M] M)
  ([M N] 
    (if (= (:width M) (:width N))
      (matrix (flatten (interleave (columns M) (columns N))) (clojure.core/+ (:height M) (:height N)) (:width M))
      nil))
  ([M N & more] 
     (reduce stack (stack M N) more)))

(defn append 
  ([M] M)
  ([M N] 
    (if (= (:height M) (:height N))
      (matrix (flatten (concat (columns M) (columns N))) (:height M) (clojure.core/+ (:width M) (:width N)))
      nil))
  ([M N & more] 
     (reduce append (append M N) more)))

(defn transpose [M] 
  (matrix (flatten (rows M)) (:width M) (:height M)))

(defn rotation-matrix
  ([angle] (matrix [(cos angle) (clojure.core/- (sin angle)) (sin angle) (cos angle)] 2 2)))

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
    (doall
      (for [row (range (:height M)) column (range (:width M))]
        (if (and (= (dec (:width M)) column) (< row (dec (:height M))))
          (print-method (format (str "%" largest-number "s\n") (str (get-cell M row column))) writer)
          (print-method (format (str "%" largest-number "s ") (str (get-cell M row column))) writer))))))

(defmethod print-method :term [x ^Writer writer]
  (.write writer
    (str
      (if (and (= (:coefficient x) 1) (not (empty? (:variables x))))
        "" 
        (:coefficient x))
      (loop [stack (:variables x) result ""]
        (if (empty? stack)
          result
          (recur (rest stack) (str result (first (first stack)) (if (= (second (first stack)) 1) "" (str "**" (second (first stack)))))))))))

(defmethod print-method :polynomial [x ^Writer writer]
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
              (second (first stack)))))))))

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

(defmulti add (fn [x y] [(:type (meta x)) (:type (meta y))]))
(defmethod add [nil :matrix] [x y] (matrix (map #(clojure.core/+ x %) (:data y)) (:height y) (:width y)))
(defmethod add [:matrix nil] [x y] (matrix (map #(clojure.core/+ % y) (:data x)) (:height x) (:width x)))
(defmethod add [:matrix :matrix] [x y] (matrix (map clojure.core/+ (:data x) (:data y)) (:height x) (:width x)))
(defmethod add [:polynomial :polynomial] [x y] 
  (apply polynomial (concat (vals x) (vals y))))
(defmethod add [:polynomial nil] [x y] 
  (apply polynomial (conj (vals x) (term y))))
(defmethod add [nil :polynomial] [x y] 
  (apply polynomial (cons (term x) (vals y))))
(defmethod add :default [x y] (clojure.core/+ x y))
  
(defn +
  ([] 0)
  ([x] x)
  ([x y] (add x y))
  ([x y & more]
    (reduce + (+ x y) more)))

(defmulti subtract (fn [x y] [(:type (meta x)) (:type (meta y))]))
(defmethod subtract [nil :matrix] [x y] (matrix (map #(clojure.core/- x %) (:data y)) (:height y) (:width y)))
(defmethod subtract [:matrix nil] [x y] (matrix (map #(clojure.core/- % y) (:data x)) (:height x) (:width x)))
(defmethod subtract [:matrix :matrix] [x y] (matrix (map clojure.core/- (:data x) (:data y)) (:height x) (:width x)))
(defmethod subtract [:polynomial :polynomial] [x y] 
    (apply polynomial (concat (vals x) (map negate-term (vals y)))))
(defmethod subtract [:polynomial nil] [x y]
  (apply polynomial (conj (vals x) (term (clojure.core/- y)))))
(defmethod subtract [nil :polynomial] [x y]
    (apply polynomial (cons (term x) (map negate-term (vals y)))))
(defmethod subtract :default [x y] (clojure.core/- x y))

(defmulti unary-subtract (fn [x] (:type (meta x))))
(defmethod unary-subtract :matrix [x] (matrix (map clojure.core/- (:data x)) (:height x) (:width x)))
(defmethod unary-subtract :default [x] (clojure.core/- x))
(defmethod unary-subtract :polynomial [x]
    (apply polynomial (map negate-term (vals x))))
  
(defn -
  ([] 0)
  ([x] (unary-subtract x))
  ([x y] (subtract x y))
  ([x y & more]
    (reduce - (- x y) more)))

(defmulti multiply (fn [x y] [(:type (meta x)) (:type (meta y))]))
(defmethod multiply [nil :matrix] [x y] (matrix (map #(clojure.core/* x %) (:data y)) (:height y) (:width y)))
(defmethod multiply [:matrix nil] [x y] (matrix (map #(clojure.core/* % y) (:data x)) (:height x) (:width x)))
(defmethod multiply [:polynomial :polynomial] [x y] 
  (apply polynomial
    (for [x-term (vals x) y-term (vals y)]
      (multiply-terms x-term y-term))))
(defmethod multiply [:polynomial nil] [x y] 
  (apply polynomial 
    (for [x-term (vals x)]
      (assoc x-term :coefficient (clojure.core/* (:coefficient x-term) y)))))
(defmethod multiply [nil :polynomial] [x y] 
  (apply polynomial 
    (for [y-term (vals y)]
      (assoc y-term :coefficient (clojure.core/* (:coefficient y-term) x)))))
(defmethod multiply [:matrix :matrix] [x y] 
  (matrix 
    (for [column (columns y) row (rows x)]
      (reduce clojure.core/+ (map clojure.core/* row column)))
    (:height x)
    (:width y)))
(defmethod multiply :default [x y] (clojure.core/* x y))

(defn * 
  ([] 1)
  ([x] x)
  ([x y] (multiply x y))
  ([x y & more]
    (reduce * (* x y) more)))

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

(defmulti unary-divide (fn [x] (:type (meta x))))
(defmethod unary-divide :matrix [x] (clojure.core// (transpose (cofactor-matrix x)) (determinant x)))
(defmethod unary-divide :default [x] (clojure.core// x))

(defn / 
  ([] 1)
  ([x] (unary-divide x))
  ([x y] (* x (clojure.core// y)))
  ([x y & more]
    (reduce / (/ x y) more)))

(defn polynomial-less-than [x y]
  ;(println "POLY" (meta x) "," (meta y))
  (let [sorter 
         (fn [x] (get (:variables x) epsilon 0))
        x-terms
         (sort-by sorter 
           (filter #(or (empty? (:variables %)) (contains? (:variables %) 'epsilon)) 
             (vals x)))
        y-terms
         (sort-by sorter 
           (filter #(or (empty? (:variables %)) (contains? (:variables %) 'epsilon)) 
                   (vals y)))]
    ;(println x-terms y-terms)
    (loop [x-stack x-terms y-stack y-terms]
      (cond 
        (and (empty? x-stack) (empty? y-stack))
        false
        (empty? x-stack)
        (> (:coefficient (first y-stack)) 0)
        (empty? y-stack)
        (< (:coefficient (first x-stack)) 0)
        true
        (let [x-epsilon-power (get (:variables (first x-stack)) 'epsilon) y-epsilon-power (get (:variables (first y-stack)) 'epsilon)]
          (cond
            (= [false false] [(number? x-epsilon-power) (number? y-epsilon-power)])
            (cond
             (> (:coefficient (first x-stack)) (:coefficient (first y-stack)))
             false
             (= (:coefficient (first x-stack)) (:coefficient (first y-stack)))
             (recur (rest x-stack) (rest y-stack))
             (< (:coefficient (first x-stack)) (:coefficient (first y-stack)))
             true)
            (= [true false] [(number? x-epsilon-power) (number? y-epsilon-power)])
            (> (:coefficient (first y-stack)) 0)
            (= [false true] [(number? x-epsilon-power) (number? y-epsilon-power)])
            (< (:coefficient (first x-stack)) 0)
            (= [true true] [(number? x-epsilon-power) (number? y-epsilon-power)])
            (cond 
              (= x-epsilon-power y-epsilon-power)
              (cond
               (> (:coefficient (first x-stack)) (:coefficient (first y-stack)))
               false
               (= (:coefficient (first x-stack)) (:coefficient (first y-stack)))
               (recur (rest x-stack) (rest y-stack))
               (< (:coefficient (first x-stack)) (:coefficient (first y-stack)))
               true)
              (< x-epsilon-power y-epsilon-power)
              (cond 
                (and (< (:coefficient (first x-stack)) 0) (< (:coefficient (first y-stack)) 0))
                true 
                (and (> (:coefficient (first x-stack)) 0) (< (:coefficient (first y-stack)) 0))
                false
                (and (< (:coefficient (first x-stack)) 0) (> (:coefficient (first y-stack)) 0))
                true
                (and (> (:coefficient (first x-stack)) 0) (> (:coefficient (first y-stack)) 0))
                false)
              (> x-epsilon-power y-epsilon-power)
              (cond 
                (and (< (:coefficient (first x-stack)) 0) (< (:coefficient (first y-stack)) 0))
                false
                (and (> (:coefficient (first x-stack)) 0) (< (:coefficient (first y-stack)) 0))
                false
                (and (< (:coefficient (first x-stack)) 0) (> (:coefficient (first y-stack)) 0))
                true
                (and (> (:coefficient (first x-stack)) 0) (> (:coefficient (first y-stack)) 0))
                true))))))))
(defn polynomial-greater-than [x y]
  (polynomial-less-than (- x) (- y)))

(defn x [a b] 
  (- (* (:x a) (:y b)) (* (:x b) (:y a))))

(defn length-squared [input] (reduce + (map #(* % %) (:data input))))
(defn length [input] (sqrt (length-squared input)))
(defn unit [input] (/ input (length input)))
(defn normal [input] 
  (if (= (size input) 2)
    (unit (column (get-cell input 1) (- (get-cell input 0))))
    nil))
(defn transform [trans point] 
  (+ (:translation trans)
    (* (:rotation trans) point)))
(defn translate [original trans] 
  (transformation (-> original :translation (+ trans)) (:rotation original)))
(defn rotate [original point angle] 
  (transformation (+ (* angle (-> original :translation (- point))) point) (-> original :rotation (* angle))))

(defstruct lcp-step :moved-out :equations)
(defstruct linear-equation :left :right)

(defn min-index [items]
  ;(println "min-index123" (map meta items))
  (loop [stack items position 0 minimum (polynomial (term positive-infinity))]
    ;(println "gfsdgfd")
    ;(println "gfdsag" (first stack))
    (if (empty? stack)
      position
      (if (polynomial-less-than (first stack) minimum)
        (recur (rest stack) (- (count items) (count stack)) (first stack))
        (recur (rest stack) position minimum)))))

(defn max-index [items]
  ;(println "max-index" items)
  (loop [stack items position 0 minimum (polynomial (term negative-infinity))]
    ;(println (first stack))
    (if (empty? stack)
      position
      (if (polynomial-greater-than (first stack) minimum)
        (recur (rest stack) (- (count items) (count stack)) (first stack))
        (recur (rest stack) position minimum)))))

(defn constant-epsilon-part [input]
  (let [temp (apply polynomial
                    (filter #(or (empty? (:variables %)) (contains? (:variables %) 'epsilon)) (vals input)))]
    temp))

(defn collect [input]
  (let [temp (loop [stack input accum {}]
    (if (empty? stack)
      accum
      (recur (rest stack) (merge-with + accum {(first stack) 1}))))] (println temp) temp))
    
(defn move-in [system moving-in]
  ;(println "move-in" moving-in)
  ;(doall (for [equation (:equations system)] (println equation)))
  ;(doall (for [equation (:equations system)] (println (:constant (:right equation)))))
  ;(println moving-in)
  ;(doall (for [equation (:equations system)] (println (-> equation :right (get moving-in)))))
  (let [ratios 
         (for [equation (:equations system)]
           (if (-> equation :right (get {moving-in 1}))
             (/ (constant-epsilon-part (:right equation)) (:coefficient (get (:right equation) {moving-in 1})))
             (polynomial (term (if (= moving-in ['z 0]) positive-infinity negative-infinity)))))
        row (if (= moving-in ['z 0]) (min-index ratios) (max-index ratios))
        ;temp (println row)
        tt (doall (for [e (:equations system)] (println (:left e) "=" (:right e))))
        ;rrr (println 'gggggggg)
        ;t2          (println ((:equations system) row))
        moving-out (first (keys (:variables (first (vals (:left ((:equations system) row)))))))
        temp2 (println moving-in moving-out)
        temp2 (println 'moving-out (:left ((:equations system) row)) (dissoc (:right ((:equations system) row)) {moving-in 1}))
        moving-right 
          (/
            (- 
              (:left ((:equations system) row))
              (dissoc (:right ((:equations system) row)) {moving-in 1}))
            (:coefficient (get (:right ((:equations system) row)) {moving-in 1})))
        temp3 (println moving-right)
        moving-left (polynomial (term moving-in))
        ffffffff (println "New equations")
        new-equations
          (apply vector
            (for [equation (concat (subvec (:equations system) 0 row) [(struct linear-equation moving-left moving-right)] (subvec (:equations system) (inc row)))]
              (if (contains? (:right equation) {moving-in 1})
                (struct linear-equation 
                        (:left equation)
                        (+ (dissoc (:right equation) {moving-in 1}) (* (:coefficient (get (:right equation) {moving-in 1})) moving-right)))
                equation)))]
    (println "lcp-step done")
    (struct lcp-step moving-out new-equations)))
    

(defn solve-lcp [M q]
  (let [raw-equations
         (struct lcp-step
                 ['w 0]    
                 (apply vector
                        (for [row (range 1 (inc (:height M)))] 
                          (struct linear-equation 
                                  (polynomial (term 1 ['w row]))
                                  (apply polynomial
                                         (term (get-cell q (dec row)))
                                         (term 1 ['z 0])
                                         (for [column (range 1 (inc (:width M))) :when (not= (get-cell M (dec row) (dec column)) 0)]
                                           (term (get-cell M (dec row) (dec column)) ['z column])))))))
        counting (collect (map #(:coefficient (get % {})) (map :right (:equations raw-equations))))
        ;qqq (println (map meta (keys counting)))
        ;gfds (println "HELLO" (= 1 (get counting (apply min (keys counting)))))
        initial-equations
        (if (= 1 (get counting (apply min (keys counting))))
          raw-equations
          (struct lcp-step
                  ['w 0]
                  (loop [stack (:equations raw-equations) accum [] index 1]
                    ;(println 'epsilon index (term 'epsilon index))
                    (if (empty? stack)
                      accum
                      (recur
                       (rest stack)
                       (conj accum (struct linear-equation (:left (first stack)) (+ (:right (first stack)) (polynomial (term 'epsilon index)))))
                       (inc index))))))
        ;gfds (println "HELLO")
        ;temp (doall (for [e (:equations initial-equations)] (println (:left e) "=" (:right e))))
        lcp-solution
         (loop [equations initial-equations]
           (if (= (:moved-out equations) ['z 0])
             equations
             (recur (move-in equations (if (= (first (:moved-out equations)) 'w) 
                                         ['z (second (:moved-out equations))]
                                         ['w (second (:moved-out equations))])))))
        subscript-sort
          (fn [equation]
            (first (first (keys (:left equation)))))]
    (println "lcp-solved")
    (doall (for [equation (:equations lcp-solution)] (println equation)))
    (println "lcp-solved")
    {'w (for [equation (sort-by subscript-sort (:equations lcp-solution))]
          (if (= (first (first (keys (:left equation)))) 'w)
            (get (:right equation) {})
            0))
     'z (for [equation (sort-by subscript-sort (:equations lcp-solution))]
          (if (= (first (first (keys (:left equation)))) 'z)
            (get (:right equation) {})
            0))}))
    
  
(defn solve-convex-quadratic-problem [S A b c]
  (let [M (stack (append S (transpose A)) (append (- A) (zero (:height A))))
        q (stack (- c) b)]
    (take (:width A) (:z (solve-lcp M q)))))

(def A (matrix [-1 2 3 1 -1 1] 3 2))
(def b (matrix [2 -1 3]))
(def c (matrix [1 1]))
(def M (stack (append (zero 2 2) (transpose A)) (append (- A) (zero 3 3))))
(def q (stack (- c) b))

;(println "Hello")
;(println A)
;(println q)
(println (solve-lcp M q))

(comment
  (let [s (* (transpose a) a)
        A0 (stack (- A) (A))
        b0 (stack b (c - b))
        c0 (* -2 (transpose b) A)
        K  (length-squared b)
        constraints (fn [x] (and (<= (* -1 A x) b) (<= (* A x) (- c b))))
        M (-> S (append (transpose A0)) (stack (append (- A0) (zero-matrix (rows A0)))))
        q (stack (- c0) b0)]))

23
