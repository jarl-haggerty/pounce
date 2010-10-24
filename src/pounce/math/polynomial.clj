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

(defmethod multiply [:polynomial nil] [x y]
           (apply polynomial (map #(cons (* (first %) y) (rest %)) (vals x))))
(defmethod multiply [nil :polynomial] [x y]
           (apply polynomial (map #(cons (* (first %) x) (rest %)) (vals y))))

(defn constant-part [input]
  (apply polynomial (filter #(or (= (second %) 1) (= (second %) :epsilon)) (vals input))))

(defmethod less-than [:polynomial :polynomial] [x y]
           (let [max-power (apply max (concat (map second (keys (constant-part x)))
                                              (map second (keys (constant-part y)))))
                 [x-terms y-terms] (map
                                    (fn [input]
                                      (loop [stack (sort-by #(if (number? (second %)) 0 last)
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
                                    x y)]
             (loop [x-stack x-terms y-stack y-terms]
               (cond
                (< (first x-stack) (first y-stack))
                true
                (> (first x-stack) (first y-stack))
                true
                (= (first x-stack) (first y-stack))
                (recur (rest x-stack) (rest y-stack))))))

(defmethod less-than [nil :polynomial] [x y] (< x (get y [1 1] 0)))
(defmethod less-than [:polynomial nil] [x y] (< (get x [1 1] 0) y))

(def data [(polynomial [-1/3 :epsilon 1] [-2/3 :epsilon 2] [1 :epsilon 4])
           (polynomial [-1 :epsilon 2] [1 :epsilon 4])
           (polynomial [1/2 :epsilon 4] [-3/2 1 1] [-1/2 :epsilon 3])
           (polynomial [-1 1 1] [1 :epsilon 4])
           (polynomial [1/2 :epsilon 4] [-2 1 1] [-1/2 :epsilon 5])])
