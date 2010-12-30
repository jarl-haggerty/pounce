(ns com.curious.pounce.simulation
  (:import java.awt.Color)
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:use com.curious.pounce.body
	com.curious.pounce.math.math
        com.curious.pounce.math.matrix
	com.curious.pounce.render))

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

(defn simulate [sim delta]
  (let [new-bodies (apply merge (:bodies sim)
                          (for [[k action] @(:actions sim)]
                            {k (step (merge-with +
                                                 (or (get action :body)
                                                     (get (:bodies sim) k))
                                                 {:linear-momentum (+ (* delta (:force action)) (:linear-impulse action))
                                                  :angular-momentum (+ (* delta (:torque action)) (:angular-impulse action))})
                                     delta)}))
        contacts (loop [bodies (vals new-bodies) accum ()]
                   (if-let [rest-bodies (next bodies)]
                     (recur (rest bodies) (apply concat accum (for [body2 rest-bodies]
                                                                (collision (first bodies) body2))))
                     accum))
        _ (comment (vec (for [bodies (map #(nthnext (vals new-bodies) %) (range (count new-bodies)))
                              body2 (rest bodies)]
                          (collision (first bodies) body2))))
        _ (comment 
                   G (apply stack (for [index (range (count contacts))
                                        :let [contact (contacts i)
                                              body1 (get sim (:body1 contact))
                                              body2 (get sim (:body2 contact))]]
                                    (transpose (stack (zeros 1 (* index 6))
                                                      (- (:normal contact))
                                                      (cross (- (* (:transform body1) (:point contact))
                                                                (* (:transform body1) (:center-of-mass body1)))
                                                             (:normal contact))
                                                      (:normal contact)
                                                      (cross (- (* (:transform body2) (:point contact))
                                                                (* (:transform body2) (:center-of-mass body2)))
                                                             (:normal contact))
                                                      (zeros 1 (* (- index 2) 6))))))
                   F (apply stack (for [contact contacts
                                        :let [body1 (get sim (:body1 contact))
                                              body2 (get sim (:body2 contact))]]
                                    (stack (/ (:linear-momentum body1)
                                              delta)
                                           (/ (:angular-momentum body1)
                                              delta)
                                           (/ (:linear-momentum body2)
                                              delta)
                                           (/ (:angular-momentum body2)
                                              delta))))
                   M (apply stack (for [index (range (count contacts))
                                        :let [contact (contacts i)
                                              body1 (get sim (:body1 contact))
                                              body2 (get sim (:body2 contact))]]
                                    (stack (component (* 6 (count contacts)) (* 3 index) (:mass body1))
                                           (component (* 6 (count contacts)) (+ 1 (* 3 index)) (:mass body1))
                                           (component (* 6 (count contacts)) (+ 2 (* 3 index)) (:moment-of-inertia body1))
                                           (component (* 6 (count contacts)) (+ 3 (* 3 index)) (:mass body2))
                                           (component (* 6 (count contacts)) (+ 4 (* 3 index)) (:mass body2))
                                           (component (* 6 (count contacts)) (+ 5 (* 3 index)) (:moment-of-inertia body2)))))
                   V (apply stack (for [contact contacts
                                        :let [body1 (get sim (:body1 contact))
                                              body2 (get sim (:body2 contact))]]
                                    (stack (/ (:linear-momentum body1)
                                              (:mass body1))
                                           (/ (:angular-momentum body1)
                                              (:mass body1))
                                           (/ (:linear-momentum body2)
                                              (:mass body2))
                                           (/ (:angular-momentum body2)
                                              (:mass body2))))))]
    (comment (if (seq contacts)
               (let [b (get-body sim (:body2 (first contacts)))]
                 (println (apply min-key y (:points (* (:transform b) (first (:shapes b))))))
                 (println contacts))))
    (assoc sim
      :bodies new-bodies
      :actions (atom (into {} (map #(vector % default-action) (keys new-bodies)))))))

(defmethod render :simulation [sim g]
           (.setColor g Color/black)
           (.fillRect g 0 0 (-> g .getClipBounds .getWidth) (-> g .getClipBounds .getHeight))
           (doseq [x (vals (:bodies sim))]
             (render x g)))
