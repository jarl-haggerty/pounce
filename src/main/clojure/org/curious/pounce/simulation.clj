(ns org.curious.pounce.simulation
  (:import java.awt.Color)
  (:use org.curious.pounce.body
	org.curious.pounce.math.math
        org.curious.pounce.math.matrix
	org.curious.pounce.render
        org.curious.pounce.collision))

(def default-action {:force 0 :torque 0 :linear-impulse 0 :angular-impulse 0})

(def next-id (atom 0))
  
(defn action
  ([sim input]
     (action sim @next-id input)
     (swap! next-id inc))
  ([sim id input]
     (let [old-action  (or (get @(:actions sim) id)
                           default-action)
           input-action (merge default-action input)
           new-action (assoc (merge-with + (dissoc old-action :body) (dissoc input-action :body))
                        :body (or (:body input-action)
                                  (:body old-action)))]
       (swap! (:actions sim) (fn [x] (assoc x id new-action))))))

(defn simulation
  ([] (simulation []))
  ([bodies] (simulation bodies (matrix 0 0)))
  ([bodies gravity] (with-meta {:bodies (into {} (map #(vector (first %) (assoc (second %) :id (first %))) bodies))
                                :gravity gravity
                                :actions (atom (into {} (map #(vector % default-action) (keys bodies))))}
                      {:type :simulation})))

(defn get-body [sim at] (get (:bodies sim) at))

(defn simulate
  ([sim delta] (simulate sim delta {}))
  ([sim delta perturbations]
     (let [merge-function (fn [x y] (if (number? x)
                                     (+ x y)
                                     (matrix/add x y)))
           new-bodies (filter #(-> % second :live) (merge-with merge-function (:bodies sim) perturbations))
           contacts (loop [bodies (vals new-bodies) accum ()]
                       (if-let [rest-bodies (next bodies)]
                         (recur (rest bodies) (concat accum (map #(body/collisions (first bodies) %) rest-bodies)))
                         accum))
           G (let [G (matrix/muti-matrix 1 6 (count contacts) (count contacts))]
               (doseq [index (-> contacts count range)
                       :let [contact (contacts index)
                             body1 (get sim (:body1 contact))
                             body2 (get sim (:body2 contact))
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
           F (let [F (matrix/create 1 (* 6 (count contacts)))]
               (doseq [index (-> contacts count range)
                       :let [contact (contacts index)
                             body1 (get sim (:body1 contact))
                             body2 (get sim (:body2 contact))]]
                 (matrix/set F (* index 6) 0 (matrix/add (get-in perturbations (:body1 contact) :force)
                                                         (matrix/div (get-in perturbations (:body1 contact) :linear-impulse) delta)))
                 (matrix/set F (+ (* index 6) 2) 0 (+ (get-in perturbations (:body1 contact) :torque)
                                                      (/ (get-in perturbations (:body1 contact) :angular-impulse) delta)))
                 (matrix/set F (+ (* index 6) 3) 0 (matrix/add (get-in perturbations (:body2 contact) :force)
                                                               (matrix/div (get-in perturbations (:body2 contact) :linear-impulse) delta)))
                 (matrix/set F (+ (* index 6) 5) 0 (+ (get-in perturbations (:body2 contact) :torque)
                                                      (/ (get-in perturbations (:body2 contact) :angular-impulse) delta)))))
           V (let [V (matrix/create 1 (* 6 (count contacts)))]
               (doseq [index (-> contacts count range)
                       :let [contact (contacts index)
                             body1 (get sim (:body1 contact))
                             body2 (get sim (:body2 contact))]]
                 (matrix/set F (* index 6) 0 (:linear-velocity body1))
                 (matrix/set F (+ (* index 6) 2) 0 (:angular-velocity body1))
                 (matrix/set F (+ (* index 6) 3) 0 (:linear-velocity body2))
                 (matrix/set F (+ (* index 6) 5) 0 (:angular-velocity body2))))
           M (let [M (matrix/multi-matrix 6 6 (count contacts) (count contacts))]
               (doseq [index (-> contacts count range)
                       :let [contact (contacts index)
                             body1 (get sim (:body1 contact))
                             body2 (get sim (:body2 contact))
                             temp (matrix/create 6 6)]]
                 (matrix/set temp 0 0 (:mass body1))
                 (matrix/set temp 1 1 (:mass body1))
                 (matrix/set temp 2 2 (:moment-of-inertia body1))
                 (matrix/set temp 3 3 (:mass body2))
                 (matrix/set temp 4 4 (:mass body2))
                 (matrix/set temp 5 5 (:moment-of-inertia body2))
                 (matrix/set M index index temp)))]
        (doseq [contact contacts]
          (process-contact contact))
        (assoc sim
          :bodies new-bodies
          :actions (atom (into {} (map #(vector % default-action) (keys new-bodies))))))))

(defmethod render :simulation [sim g]
           (.setColor g Color/black)
           (.fillRect g 0 0 (-> g .getClipBounds .getWidth) (-> g .getClipBounds .getHeight))
           (doseq [x (vals (:bodies sim))]
             (render x g)))
