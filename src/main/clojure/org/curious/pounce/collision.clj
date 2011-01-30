(ns org.curious.pounce.collision
  (:require [org.curious.pounce.math.core :as math]
            [org.curious.pounce.math.matrix :as matrix]
            [org.curious.pounce.body :as body]
            [org.curious.pounce.shape :as shape]))

(defmulti process-collision #(hash-set (:body1 %) (:body2 %)))

(defn collision= [this that]
  (and (math/eps= (:depth this) (:depth that))
       (= (dissoc this :depth) (dissoc that :depth))))

(defn get-shape-collision-with-separating-axis
  "Calculates all the points of collisions that will occur in the next delta seconds between two bodies, neglecting rotation."
  ([shape1 shape2]
     (if-let [direction1 (get-shape-collision-with-separating-axis shape1 shape2 (matrix/sub (:center shape2) (:center shape1)))]
       (if-let [direction2 (get-shape-collision-with-separating-axis shape2 shape1 (matrix/sub (:center shape1) (:center shape2)))]
         (if (math/eps<= (-> direction1 first :depth)
                         (-> direction2 first :depth))
           direction1
           direction2)
         nil)
       nil))
  ([shape1 shape2 direction]
     (loop [n-stack (shape/normals shape1 direction) accum [{:depth math/positive-infinity}]]
       (if-let [n (first n-stack)]
         (let [proj (shape/projection (shape/translate shape2 (-> n :side first matrix/sub)) (:normal n))
               start-points (:start-points proj)]
           (if (math/eps> (:start proj) 0)
             nil
             (let [depth (- (:start proj))
                   default-contact {:normal (:normal n) :depth depth}]
               (cond
                (math/eps<= (:depth (first accum)) depth)
                (recur (rest n-stack) accum)
                (= (count start-points) 1)
                (let [plane (matrix/unit (matrix/sub (second (:side n))
                                                     (first (:side n))))
                      side {:min 0 :min-point (first (:side n))
                            :max (matrix/length (matrix/sub (second (:side n)) (first (:side n)))) :max-point (second (:side n))}
                      point (matrix/dot plane (first start-points))]
                  (if (and (math/eps< (:min side) point) (math/eps< point (:max side)))
                    (recur (rest n-stack) [(merge default-contact {:point (matrix/add (matrix/mul plane point) (first (:side n)))})])
                    (recur (rest n-stack) accum)))
                :else
                (let [plane (matrix/unit (matrix/sub (second (:side n))
                                                     (first (:side n))))
                      side1 {:min 0 :min-point (first (:side n))
                             :max (matrix/length (matrix/sub (second (:side n)) (first (:side n)))) :max-point (second (:side n))}
                      side2 (let [first-dot (matrix/dot plane (first start-points))
                                  second-dot (matrix/dot plane (second start-points))
                                  temp (sort-by first
                                                [[first-dot (matrix/add (matrix/mul plane first-dot) (first (:side n)))]
                                                 [second-dot (matrix/add (matrix/mul plane second-dot) (first (:side n)))]])]
                              {:min (first (first temp)) :min-point (second (first temp))
                               :max (first (second temp)) :max-point (second (second temp))})]
                  (cond
                   (or (and (math/eps< (:min side1) (:min side2)) (math/eps< (:max side2) (:max side1)))
                       (and (math/eps= (:min side1) (:min side2)) (math/eps< (:max side2) (:max side1)))
                       (and (math/eps< (:min side1) (:min side2)) (math/eps= (:max side2) (:max side1))))
                   (recur (rest n-stack) (map (fn [x] (merge default-contact {:point x :face1 (:side n) :face2 (map #(matrix/add % (-> n :side first)) start-points)}))
                                              [(:min-point side2) (:max-point side2)]))
                   (or (and (math/eps= (matrix/x (:normal n)) 0)
                            (math/eps> (matrix/y (:normal n)) 0))
                       (math/eps> (matrix/x (:normal n)) 0))
                   (cond
                    (math/eps= (:min side1) (:max side2))
                    (recur (rest n-stack) [(merge default-contact {:point (:min-point side1)})])
                    (math/eps= (:max side1) (:min side2))
                    (recur (rest n-stack) [(merge default-contact {:point (:max-point side1)})])
                    (and (math/eps= (:min side1) (:min side2)) (math/eps= (:max side2) (:max side1)))
                    (recur (rest n-stack) (map (fn [x] (merge default-contact {:point x :face1 (:side n) :face2 (map #(matrix/add % (-> n :side first)) start-points)}))
                                               [(:min-point side2) (:max-point side2)]))
                    (and (math/eps< (:min side1) (:min side2)) (math/eps< (:min side2) (:max side1)) (math/eps< (:max side1) (:max side2)))
                    (recur (rest n-stack) (map (fn [x] (merge default-contact {:point x :face1 (:side n) :face2 (map #(matrix/add % (-> n :side first)) start-points)}))
                                               [(:min-point side2) (:max-point side1)]))
                    (and (math/eps< (:min side2) (:min side1)) (math/eps< (:min side1) (:max side2)) (math/eps< (:max side2) (:max side1)))
                    (recur (rest n-stack) (map (fn [x] (merge default-contact {:point x :face1 (:side n) :face2 (map #(matrix/add % (-> n :side first)) start-points)}))
                                               [(:min-point side1) (:max-point side2)]))
                    :else
                    (recur (rest n-stack) accum))
                   :else
                   (recur (rest n-stack) accum)))))))
         accum))))

(defn get-body-collisions-with-separating-axis [this that]
  (map #(assoc %
          :body1 (:id this)
          :body2 (:id that))
       (->> (for [shape1 (:shapes this)
                  shape2 (:shapes that)]
              (get-shape-collision-with-separating-axis (shape/transform shape1 (:transformation this))
                               (shape/transform shape2 (:transformation that))))
            flatten
            (filter identity))))

(defn separating-axis-collision-detection [bodies]
  (loop [body-stack bodies accum ()]
    (if-let [rest-bodies (next body-stack)]
      (recur rest-bodies (concat accum (flatten (map #(get-body-collisions-with-separating-axis (first body-stack) %) rest-bodies))))
      (vec accum))))

(defn velocity-based-collision-response [bodies contacts perturbations delta]
  (let [merge-function (fn [x y] (if (number? x)
                                  (+ x y)
                                  (matrix/add x y))
                         x y)
        G (let [G (matrix/multi-matrix 1 6 (count contacts) (count contacts))]
            (doseq [index (-> contacts count range)
                    :let [contact (contacts index)
                          normal (:normal contact)
                          negative-normal (matrix/sub normal)
                          body1 (get bodies (:body1 contact))
                          body2 (get bodies (:body2 contact))]]
              (matrix/batch-set G index index (matrix/row-matrix (matrix/x negative-normal)
                                                                 (matrix/y negative-normal)
                                                                 (- (matrix/cross (matrix/sub (:point contact)
                                                                                              (matrix/transform (:center-of-mass body1) (:transformation body1)))
                                                                                  (:normal contact)))
                                                                 (matrix/x normal)
                                                                 (matrix/y normal)
                                                                 (matrix/cross (matrix/sub (:point contact)
                                                                                           (matrix/transform (:center-of-mass body2) (:transformation body2)))
                                                                               (:normal contact)))))
            G)
        F (let [F (matrix/allocate (* 6 (count contacts)))]
            (doseq [index (-> contacts count range)
                    :let [contact (contacts index)
                          body1 (get perturbations (:body1 contact))
                          body2 (get perturbations (:body2 contact))
                          body1-force (matrix/add (get body1 :force matrix/zero) (matrix/div (get body1 :linear-impulse matrix/zero) delta))
                          body1-torque (+ (get body1 :torque 0) (/ (get body1 :angular-impulse 0) delta))
                          body2-force (matrix/add (get body2 :force matrix/zero) (matrix/div (get body2 :linear-impulse matrix/zero) delta))
                          body2-torque (+ (get body2 :torque 0) (/ (get body2 :angular-impulse 0) delta))]]
              (matrix/set F (* index 6) 0 (matrix/x body1-force))
              (matrix/set F (+ (* index 6) 1) 0 (matrix/y body1-force))
              (matrix/set F (+ (* index 6) 2) 0 body1-torque)
              (matrix/set F (+ (* index 6) 3) 0 (matrix/x body2-force))
              (matrix/set F (+ (* index 6) 4) 0 (matrix/y body2-force))
              (matrix/set F (+ (* index 6) 5) 0 body2-torque))
            F)
        V (let [V (matrix/allocate (* 6 (count contacts)) 1)]
            (doseq [index (-> contacts count range)
                    :let [contact (contacts index)
                          body1 (get bodies (:body1 contact))
                          body2 (get bodies (:body2 contact))]]
              (matrix/set V (* index 6) 0 (matrix/x (:linear-velocity body1)))
              (matrix/set V (+ (* index 6) 1) 0 (matrix/y (:linear-velocity body1)))
              (matrix/set V (+ (* index 6) 2) 0 (:angular-velocity body1))
              (matrix/set V (+ (* index 6) 3) 0 (matrix/x (:linear-velocity body2)))
              (matrix/set V (+ (* index 6) 4) 0 (matrix/y (:linear-velocity body2)))
              (matrix/set V (+ (* index 6) 5) 0 (:angular-velocity body2)))
            V)
        M-inverse (let [M-inverse (matrix/allocate-diagonal-matrix (* 6 (count contacts)))]
                    (doseq [index (-> contacts count range)
                            :let [contact (contacts index)
                                  body1 (get bodies (:body1 contact))
                                  body2 (get bodies (:body2 contact))]]
                      (matrix/set M-inverse (* index 6) (* index 6) (/ (:mass body1)))
                      (matrix/set M-inverse (+ (* index 6) 1) (+ (* index 6) 1) (/ (:mass body1)))
                      (matrix/set M-inverse (+ (* index 6) 2) (+ (* index 6) 2) (/ (:moment-of-inertia body1)))
                      (matrix/set M-inverse (+ (* index 6) 3) (+ (* index 6) 3) (/ (:mass body2)))
                      (matrix/set M-inverse (+ (* index 6) 4) (+ (* index 6) 4) (/ (:mass body2)))
                      (matrix/set M-inverse (+ (* index 6) 5) (+ (* index 6) 5) (/ (:moment-of-inertia body2))))
                    M-inverse)
        _ (println G)
        _ (println M-inverse)
        G-M-inverse (matrix/mul G M-inverse)
        G-transpose (matrix/transpose G)
        _ (println G-M-inverse)
        _ (println G-transpose)
        left (matrix/mul G-M-inverse G-transpose)
                                        ;maybe I'll add constraint bias someday
        right (matrix/sub (matrix/mul G V (/ (- delta))) (matrix/mul G-M-inverse F))
        F-hat (matrix/mul G-transpose (matrix/gauss-seidel left right))]
    (println "lambda" (matrix/gauss-seidel left right))
    (println "F-hat" F-hat) (comment
                      (apply merge-with merge-function bodies
                             (for [index (-> contacts count range)
                                   :let [contact (contacts index)
                                         body1 (get-in perturbations (:body1 contact))
                                         body2 (get-in perturbations (:body2 contact))]]
                               {(:body1 contact) {:external-force (+ (matrix/create (+ (matrix/get F-hat (* index 6) 0) (matrix/get F (* index 6) 0))
                                                                                    (+ (matrix/get F-hat (+ (* index 6) 1) 0) (matrix/get F (+ (* index 6) 1) 0))))
                                                  :external-torque (+ (matrix/get F-hat (+ (* index 3) 2) 0) (matrix/get F (+ (* index 3) 2) 0))}
                                (:body2 contact) {:external-force (+ (matrix/create (+ (matrix/get F-hat (+ (* index 6) 3) 0) (matrix/get F (+ (* index 6) 3) 0))
                                                                                    (+ (matrix/get F-hat (+ (* index 6) 4) 0) (matrix/get F (+ (* index 6) 4) 0))))
                                                  :external-torque (+ (matrix/get F-hat (+ (* index 3) 5) 0) (matrix/get F (+ (* index 3) 5) 0))}})))))
