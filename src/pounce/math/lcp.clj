(ns pounce.math.lcp
  (:use pounce.math.polynomial
        pounce.math.matrix
        pounce.math.operations))

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
