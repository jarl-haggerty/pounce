"
Copyright 2010 Jarl Haggerty

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
       
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"

(ns org.curious.pounce.simulation
  (:import java.awt.Color
           java.awt.Graphics)
  (:require [org.curious.pounce.body :as body]
            [org.curious.pounce.math.core :as math]
            [org.curious.pounce.math.matrix :as matrix]
            [org.curious.pounce.render :as render]))

(def next-id (atom -1))
(defn get-unique-id [] (swap! next-id inc))

(defrecord Simulation [bodies]
  render/Renderable
  (render [this ^Graphics graphics]
          (.setColor graphics Color/black)
          (.fillRect graphics 0 0 (-> graphics .getClipBounds .getWidth) (-> graphics .getClipBounds .getHeight))
          (doseq [x (vals (:bodies sim))]
            (render x graphics))))

(defn simulation
  ([] (simulation {}))
  ([bodies] (Simulation. bodies)))

(defn simulate
  ([sim delta] (simulate sim delta {}))
  ([sim delta perturbations]
     (let [merge-function (fn [x y] (merge-with #(cond (number? %1) (+ %1 %2)
                                                      (matrix/matrix? %1) (matrix/add %1 %2)
                                                      :else %1)
                                               x y))
           new-bodies (into {} (filter #(-> % second :live) (merge-with merge-function (:bodies sim) perturbations)))
           contacts (loop [bodies (vals new-bodies) accum ()]
                       (if-let [rest-bodies (next bodies)]
                         (recur (rest bodies) (concat accum (flatten (map #(body/collisions (first bodies) %) rest-bodies))))
                         (vec accum)))
           G (let [G (matrix/multi-matrix 1 6 (count contacts) (count contacts))]
               (doseq [index (-> contacts count range)
                       :let [contact (contacts index)
                             body1 (get-in sim :bodies (:body1 contact))
                             body2 (get-in sim :bodies (:body2 contact))
                             temp (matrix/create 1 6)]]
                 (matrix/set temp 0 0 (- (:normal contact)))
                 (matrix/set temp 0 2 (matrix/cross (matrix/sub (matrix/transform (:point contact) (:transform body1))
                                                                (matrix/transform (:center-of-mass body1) (:transform body1)))
                                                    (:normal contact)))
                 (matrix/set temp 0 3 (:normal contact))
                 (matrix/set temp 0 5 (matrix/cross (matrix/sub (matrix/transform (:point contact) (:transform body2))
                                                                (matrix/transform (:center-of-mass body2) (:transform body2)))
                                                    (:normal contact)))
                 (matrix/set G index index temp)))
           G-transpose (matrix/transpose G)
           F (let [F (matrix/allocate (* 6 (count contacts)))]
               (doseq [index (-> contacts count range)
                       :let [contact (contacts index)
                             body1 (get-in sim :bodies (:body1 contact))
                             body2 (get-in sim :bodies (:body2 contact))]]
                 (matrix/set F (* index 6) (matrix/x (:external-force body1)))
                 (matrix/set F (+ (* index 6) 1) (matrix/y (:external-force body1)))
                 (matrix/set F (+ (* index 6) 2) 0 (:external-torque body1))
                 (matrix/set F (+ (* index 6) 3) (matrix/x (:external-force body2)))
                 (matrix/set F (+ (* index 6) 4) (matrix/y (:external-force body2)))
                 (matrix/set F (+ (* index 6) 5) 0 (:external-torque body2))))
           V (let [V (matrix/create 1 (* 6 (count contacts)))]
               (doseq [index (-> contacts count range)
                       :let [contact (contacts index)
                             body1 (get-in sim :bodies :bodies (:body1 contact))
                             body2 (get-in sim :bodies (:body2 contact))]]
                 (matrix/set F (* index 6) (matrix/x (:linear-velocity body1)))
                 (matrix/set F (+ (* index 6) 1) (matrix/y (:linear-velocity body1)))
                 (matrix/set F (+ (* index 6) 2) 0 (:angular-velocity body1))
                 (matrix/set F (+ (* index 6) 3) (matrix/x (:linear-velocity body2)))
                 (matrix/set F (+ (* index 6) 4) (matrix/y (:linear-velocity body2)))
                 (matrix/set F (+ (* index 6) 5) 0 (:angular-velocity body2))))
           M-inverse (let [M (matrix/multi-matrix 6 6 (count contacts) (count contacts))]
                       (doseq [index (-> contacts count range)
                               :let [contact (contacts index)
                                     body1 (get-in sim :bodies (:body1 contact))
                                     body2 (get-in sim :bodies (:body2 contact))
                                     temp (matrix/create 6 6)]]
                         (matrix/set temp 0 0 (/ (:mass body1)))
                         (matrix/set temp 1 1 (/ (:mass body1)))
                         (matrix/set temp 2 2 (/ (:moment-of-inertia body1)))
                         (matrix/set temp 3 3 (/ (:mass body2)))
                         (matrix/set temp 4 4 (/ (:mass body2)))
                         (matrix/set temp 5 5 (/ (:moment-of-inertia body2)))
                         (matrix/set M index index temp)))
           left (matrix/mul G M-inverse (matrix/transpose G))
           ;maybe I'll add constraint bias someday
           right (matrix/sub (matrix/mul G V (/ -delta)) (matrix/mul G M-inverse F))
           constraint-force (matrix/mul G-transpose (matrix/gauss-seidel left right))
           constrained-bodies (apply merge-with merge-function new-bodies
                                     (for [index (-> contacts count range)
                                           :let [contact (contacts index)
                                                 body1 (get-in sim :bodies (:body1 contact))
                                                 body2 (get-in sim :bodies (:body2 contact))]]
                                       {(:body1 contact) {:force (matrix/create (matrix/get constraint-force (* index 3))
                                                                                (matrix/get constraint-force (+ (* index 3) 1)))
                                                          :torque (matrix/get constraint-force (+ (* index 3) 2))}
                                        (:body2 contact) {:force (matrix/create (- (matrix/get constraint-force (* index 3)))
                                                                                (- (matrix/get constraint-force (+ (* index 3) 1))))
                                                          :torque (- (matrix/get constraint-force (+ (* index 3) 2)))}}))]
       (doseq [contact contacts]
         (process-contact contact))
       (assoc sim
         :bodies (reduce conj {} (map #(vector (first %) (-> % second body/update))))))))
