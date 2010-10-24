(ns pounce.math.polynomial
  (:use pounce.math.core)
  (:refer-clojure :exclude [+ - * / < <= > >=]))

(defn polynomial [& terms]
  (let [new-poly
         (into {}
           (filter #(not= (first (second %)) 0)
             (apply merge-with #(cons (+ (first %1) (first %2)) (rest %1))
                    (for [temp terms]
                      (if (number? temp)
                        {[1 1] temp}
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
              (first (second (first stack)))
              (if (number? (first (first (first stack)))) "" (first (first (first stack))))
              "**" (second (first (first stack))))))))))

(defmethod add [:polynomial :polynomial] [x y] 
           (apply polynomial (concat (vals x) (vals y))))
(defmethod add [:polynomial nil] [x y] 
           (apply polynomial (conj (vals x) [y 1 1])))
(defmethod add [nil :polynomial] [x y] 
           (apply polynomial (cons [x 1 1] (vals y))))

(defmethod negate :polynomial [x]
    (apply polynomial (map #(cons (- (first %)) (rest %)) (vals x))))

(defmethod less-than [:polynomial :polynomial] [x y]
  (let [x-terms
         (sort-by #(if (= (second %) :epsilon) (first %) 0) 
           (filter #(or (= (second %) 1) (= (second %) :epsilon)) 
                   (vals x)))
        y-terms
         (sort-by #(if (= (second %) :epsilon) (first %) 0)
           (filter #(or (= (second %) 1) (= (second %) :epsilon)) 
                   (vals y)))]
    (loop [x-stack x-terms y-stack y-terms]
      (cond 
        (and (empty? x-stack) (empty? y-stack))
        false
        (empty? x-stack)
        (> (first (first y-stack)) 0)
        (empty? y-stack)
        (< (first (first x-stack)) 0)
        true
        (let [x-epsilon-power (if (= (second (first x-stack)) :epsilon) (last (first x-stack)) nil)
              y-epsilon-power (if (= (second (first y-stack)) :epsilon) (last (first y-stack)) nil)]
          (cond
            (= [false false] [(number? x-epsilon-power) (number? y-epsilon-power)])
            (cond
             (> (first (first x-stack)) (first (first y-stack)))
             false
             (= (first (first x-stack)) (first (first y-stack)))
             (recur (rest x-stack) (rest y-stack))
             (< (first (first x-stack)) (first (first y-stack)))
             true)
            (= [true false] [(number? x-epsilon-power) (number? y-epsilon-power)])
            (> (first (first y-stack)) 0)
            (= [false true] [(number? x-epsilon-power) (number? y-epsilon-power)])
            (< (first (first x-stack)) 0)
            (= [true true] [(number? x-epsilon-power) (number? y-epsilon-power)])
            (cond 
              (= x-epsilon-power y-epsilon-power)
              (cond
               (> (first (first x-stack)) (first (first y-stack)))
               false
               (= (first (first x-stack)) (first (first y-stack)))
               (recur (rest x-stack) (rest y-stack))
               (< (first (first x-stack)) (first (first y-stack)))
               true)
              (< x-epsilon-power y-epsilon-power)
              (cond 
                (and (< (first (first x-stack)) 0) (< (first (first y-stack)) 0))
                true 
                (and (> (first (first x-stack)) 0) (< (first (first y-stack)) 0))
                false
                (and (< (first (first x-stack)) 0) (> (first (first y-stack)) 0))
                true
                (and (> (first (first x-stack)) 0) (> (first (first y-stack)) 0))
                false)
              (> x-epsilon-power y-epsilon-power)
              (cond 
                (and (< (first (first x-stack)) 0) (< (first (first y-stack)) 0))
                false
                (and (> (first (first x-stack)) 0) (< (first (first y-stack)) 0))
                false
                (and (< (first (first x-stack)) 0) (> (first (first y-stack)) 0))
                true
                (and (> (first (first x-stack)) 0) (> (first (first y-stack)) 0))
                true))))))))
(defmethod less-than [nil :polynomial] [x y] (< x (get y [1 1] 0)))
(defmethod less-than [:polynomial nil] [x y] (< (get x [1 1] 0) y))

(defn constant-part [input]
  (apply polynomial (filter #(or (= (second %) 1) (= (second %) :epsilon)) (vals input))))
