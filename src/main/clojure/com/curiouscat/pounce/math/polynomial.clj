(ns com.curiouscat.pounce.math.polynomial
  (:refer-clojure :exclude [+ - * / < <= > >= max-key min-key])
  (:use com.curiouscat.pounce.math.math
        clojure.test))

(defn polynomial [& terms]
  (let [new-poly
         (into {}
           (filter #(not= (first (second %)) 0)
             (apply merge-with #(cons (+ (first %1) (first %2)) (rest %1))
                    (for [temp terms]
                      (if (not (sequential? temp))
                        (if (number? temp) {[1 1] [temp 1 1]} {[temp 1] [1 temp 1]})
                        (if (= (count temp) 3)
                          {(rest temp) temp}
                          (if (number? (first temp))
                            {[(second temp) 1] [(first temp) (second temp) 1]}
                            {temp (cons 1 temp)})))))))]
    (with-meta new-poly {:type :polynomial})))
(comment
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
                              (str "**" (second (first (first stack)))))))))))))

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
  (apply polynomial (filter #(or (= (second %) 1) (= (second %) 'epsilon)) (vals input))))

(defmethod less-than [:polynomial :polynomial] [x y]
           (let [max-power (apply max (concat (map second (keys (constant-part x)))
                                              (map second (keys (constant-part y)))))
                 [x-terms y-terms] (map
                                    (fn [input]
                                      (loop [stack (sort-by #(if (number? (second %)) 0 (last %))
                                                            (vals (constant-part input)))
                                             index (range (inc max-power))
                                             accum []]
                                        (if (empty? index)
                                            accum
                                          (if (number? (second (first stack)))
                                            (recur (rest stack) (rest index) (conj accum (first (first stack))))
                                            (if (= (last (first stack)) (first index))
                                              (recur (rest stack) (rest index) (conj accum (first (first stack))))
                                              (recur stack (rest index) (conj accum 0)))))))
                                    [x y])]
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

(defmethod less-than [nil :polynomial] [x y] (less-than (polynomial x) y))
(defmethod less-than [:polynomial nil] [x y] (less-than x (polynomial y)))

(deftest polynomial-1 (is (= (polynomial 3) {[1 1] [3 1 1]})))
(deftest polynomial-2 (is (= (polynomial [2 'x]) {['x 1] [2 'x 1]})))
(deftest polynomial-3 (is (= (polynomial ['x 3]) {['x 3] [1 'x 3]})))
(deftest polynomial-4 (is (= (polynomial [2 'x 3]) {['x 3] [2 'x 3]})))
(deftest polynomial-5 (is (= (polynomial 3 [2 'x] ['x 3] [2 'x 3]) {[1 1] [3 1 1] ['x 1] [2 'x 1] ['x 3] [3 'x 3]})))
(deftest polynomial-add-1 (is (= (+ (polynomial 3 [2 'x 3] [8 'x 4]) (polynomial 4 ['x 4]))
                                 {[1 1] [7 1 1] ['x 3] [2 'x 3] ['x 4] [9 'x 4]})))
(deftest polynomial-add-2 (is (= (+ (polynomial [2 'x 3] [8 'x 4]) 5)
                                 {[1 1] [5 1 1] ['x 3] [2 'x 3] ['x 4] [8 'x 4]})))
(deftest polynomial-add-3 (is (= (+ 5 (polynomial 4 ['x 4]))
                                 {[1 1] [9 1 1] ['x 4] [1 'x 4]})))
(deftest polynomial-negate-test (is (= (- (polynomial 3 [2 'x 3] [8 'x 4])) {[1 1] [-3 1 1] ['x 3] [-2 'x 3] ['x 4] [-8 'x 4]})))
(deftest polynomial-multiply-2 (is (= (* (polynomial [2 'x 3] [8 'x 4]) 5)
                                 {['x 3] [10 'x 3] ['x 4] [40 'x 4]})))
(deftest polynomial-multiply-3 (is (= (* 5 (polynomial 4 ['x 4]))
                                 {[1 1] [20 1 1] ['x 4] [5 'x 4]})))
(deftest polynomial-constant-1 (is (= (constant-part (polynomial [2 'x 3])) {})))
(deftest polynomial-constant-2 (is (= (constant-part (polynomial 2)) {[1 1] [2 1 1]})))
(deftest polynomial-constant-3 (is (= (constant-part (polynomial 'epsilon)) {['epsilon 1] [1 'epsilon 1]})))
(deftest polynomial-constant-4 (is (= (constant-part (polynomial 2 [2 'x 3])) {[1 1] [2 1 1]})))
(deftest polynomial-constant-5 (is (= (constant-part (polynomial 2 'epsilon)) {[1 1] [2 1 1] ['epsilon 1] [1 'epsilon 1]})))
(deftest polynomial-constant-6 (is (= (constant-part (polynomial 2 'epsilon [2 'x 3])) {[1 1] [2 1 1] ['epsilon 1] [1 'epsilon 1]})))
(deftest polynomial-less-than-1 (is (< (polynomial [10 'x 30] [2 'y 4]) (polynomial 5))))
(deftest polynomial-less-than-2 (is (< (polynomial [10 'x 30] [2 'y 4] 'epsilon) (polynomial 5))))
(deftest polynomial-less-than-3 (is (< (polynomial [10 'x 30] [2 'y 4]) (polynomial 'epsilon))))
(deftest polynomial-less-than-4 (is (< (polynomial [10 'x 30] [2 'y 4] ['epsilon 2]) (polynomial 'epsilon))))
(deftest polynomial-less-than-5 (is (< (polynomial [10 'x 30] [2 'y 4] ['epsilon 2] [100 'epsilon 3]) (polynomial [5'epsilon 2]))))
(deftest polynomial-less-than-6 (is (< (polynomial [10 'x 30] [2 'y 4]) 5)))
(deftest polynomial-less-than-7 (is (< 5 (polynomial 40 [10 'x 30] [2 'y 4]))))
