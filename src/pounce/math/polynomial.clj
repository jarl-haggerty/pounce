(ns pounce.math.polynomial
  (:use pounce.math.operations))

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
