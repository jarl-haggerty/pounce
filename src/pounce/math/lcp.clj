(ns pounce.math.lcp
  (:use pounce.math.core
        pounce.math.polynomial
        pounce.math.matrix)
  (:refer-clojure :exclude [+ - * / < <= > >=]))


(defn max-key
  "Returns the x for which (k x), a number, is greatest."
  {:added "1.0"}
  ([k x] x)
  ([k x y] (if (> (k x) (k y)) x y))
  ([k x y & more]
   (reduce #(max-key k %1 %2) (max-key k x y) more)))

(defn min-key
  "Returns the x for which (k x), a number, is least."
  {:added "1.0"}
  ([k x] x)
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
   (reduce #(min-key k %1 %2) (min-key k x y) more)))


(defstruct lcp-step :moved-out :equations)
(defstruct linear-equation :left :right)

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
           (if (and (get (:right equation) [moving-in 1])
                    (if (= moving-in [:z 0])
                      true
                      (> 0 (first (get (:right equation) [moving-in 1])))))
             (do
               ;(println (first (get (:right equation) [moving-in 1])))
               ;(println (constant-part (:right equation)))
               ;(println (/ (constant-part (:right equation)) (first (get (:right equation) [moving-in 1])))lambda)
               (/ (constant-part (:right equation)) (first (get (:right equation) [moving-in 1]))))
             (polynomial (if (= moving-in [:z 0]) positive-infinity negative-infinity))))
        aco (println "ratios" (interleave ratios (repeat (count ratios) ",")))
        row (if (= moving-in [:z 0])
              (apply min-key #(nth ratios %) (range (count ratios)))
              (apply max-key #(nth ratios %) (range (count ratios))))
        temp (println "row" row)
        tt (doall (for [e (:equations system)] (println (:left e) "=" (:right e))))
        ;rrr (println 'gggggggg)
        ;t2          (println ((:equations system) row))
        moving-out (first (first (keys (:left ((:equations system) row)))))
        temp2 (println moving-in moving-out)
        temp2 (println 'moving-out (:left ((:equations system) row)) (dissoc (:right ((:equations system) row)) [moving-in 1]))
        moving-right 
          (/
            (- 
              (:left ((:equations system) row))
              (dissoc (:right ((:equations system) row)) [moving-in 1]))
            (first (get (:right ((:equations system) row)) [moving-in 1])))
        temp3 (println moving-right)
        moving-left (polynomial [1 moving-in 1])
        ffffffff (println "New equations")
        new-equations
          (apply vector
            (for [equation (concat (subvec (:equations system) 0 row) [(struct linear-equation moving-left moving-right)] (subvec (:equations system) (inc row)))]
              (if (contains? (:right equation) [moving-in 1])
                (struct linear-equation 
                        (:left equation)
                        (+ (dissoc (:right equation) [moving-in 1])
                           (* (first (get (:right equation) [moving-in 1])) moving-right)))
                equation)))]
    (println "lcp-step done")
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
        counting (collect (map #(first (get % [1 1])) (map :right (:equations raw-equations))))
        ;qqq (println (map meta (keys counting)))
        ;gfds (println "HELLO" (= 1 (get counting (apply min (keys counting)))))
        initial-equations
        (if (= 1 (get counting (apply min (keys counting))))
          raw-equations
          (struct lcp-step
                  [:w 0]
                  (loop [stack (:equations raw-equations) accum [] index 1]
                    ;(println 'epsilon index (term 'epsilon index))
                    (if (empty? stack)
                      accum
                      (recur
                       (rest stack)
                       (conj accum (struct linear-equation
                                           (:left (first stack))
                                           (+ (:right (first stack)) (polynomial [:epsilon index]))))
                       (inc index))))))
        ;gfds (println "HELLO")
        ;temp (doall (for [e (:equations initial-equations)] (println (:left e) "=" (:right e))))
        lcp-solution
         (loop [equations initial-equations]
           (if (= (:moved-out equations) [:z 0])
             equations
             (recur (move-in equations (if (= (first (:moved-out equations)) :w) 
                                         [:z (second (:moved-out equations))]
                                         [:w (second (:moved-out equations))])))))
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


(comment
  (let [s (* (transpose a) a)
        A0 (stack (- A) (A))
        b0 (stack b (c - b))
        c0 (* -2 (transpose b) A)
        K  (length-squared b)
        constraints (fn [x] (and (<= (* -1 A x) b) (<= (* A x) (- c b))))
        M (-> S (append (transpose A0)) (stack (append (- A0) (zero-matrix (rows A0)))))
        q (stack (- c0) b0)]))
