(ns com.curious.pounce.simulation
  (:import java.awt.Color)
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:use com.curious.pounce.body
	com.curious.pounce.math.math 
	com.curious.pounce.render))
(comment
  (def default-action {:force 0 :torque 0 :linear-impulse 0 :angular-impulse 0 :body nil})

  (def next-id (atom 0))
  
  (defn action
    ([sim input]
       (action sim @next-id input)
       (swap! next-id inc))
    ([sim id input]
       (let [old-action (dissoc (get @(:actions sim) id) :body)
             input-action (dissoc (merge default-action input) :body)
             new-action (merge-with + old-action input-action)]
         (swap! (:actions sim) (fn [x] (if (nil? (:body input))
                                        new-action
                                        (assoc new-action (:body input))))))))

  (defn simulation
    ([] (simulation []))
    ([bodies] (simulation bodies (column 0 0)))
    ([bodies gravity] {:simulation bodies :gravity gravity :actions (atom {})}))

  (defn get-body [sim at] (get (:bodies sim) at))

  (defn simulate [sim delta]
    (let [new-bodies (apply merge (:bodies sim)
                            (for [k (keys @(:actions sim))]
                              (let [action (get @(:actions sim) k)]
                                {k (merge-with +
                                               (get @(:actions sim) :body (get (:bodies sim) k))
                                               {:linear-momentum (+ (* delta (:force action)) (:linear-impulse action))
                                                :angular-momentum (+ (* delta (:torque action)) (:angular-impulse action))})})))
          contacts (loop [stack1 (vals new-bodies) accum1 []]
                     (if-let [body1 (first stack1)]
                       (recur (rest stack1)
                              (concat accum1
                                      (loop [stack2 (rest stack1) accum2 []]
                                        (if-let [body2 (first stack2)]
                                          (recur (rest stack2) (concat accum2 (collision body1 body2 delta)))
                                          accum2))))))
          lcp-matrix (matrix (for [c1 contacts c2 contacts
                                   :let [ran1 (cross (- (:point c1) (* (-> c1 :body1 :transform)
                                                                       (-> c1 :body1 :center-of-mass)))
                                                     (:normal c1))
                                         rbn1 (cross (- (:point c1) (* (-> c1 :body2 :transform)
                                                                       (-> c1 :body2 :center-of-mass)))
                                                     (:normal c1))
                                         ran2 (cross (- (:point c2) (* (-> c2 :body1 :transform)
                                                                       (-> c2 :body1 :center-of-mass)))
                                                     (:normal c2))
                                         rbn2 (cross (- (:point c2) (* (-> c2 :body2 :transform)
                                                                       (-> c2 :body2 :center-of-mass)))
                                                     (:normal c2))
                                         first-term (+ (/ (dot (:normal c1) (:normal c2)) (-> c1 :body1 :mass))
                                                       (dot ran1 (/ ran2 (-> c1 :body1 :moment-of-inertia))))
                                         second-term (+ (/ (dot (:normal c1) (:normal c2)) (-> c1 :body2 :mass))
                                                        (dot rbn1 (/ rbn2 (-> c1 :body2 :moment-of-inertia))))]]
                               (+ (cond
                                   (= (:body1 c1) (:body1 c2)) first-term
                                   (= (:body1 c1) (:body2 c2)) (- first-term)
                                   :else 0)
                                  (cond
                                   (= (:body2 c1) (:body1 c2)) first-term
                                   (= (:body2 c1) (:body2 c2)) (- first-term)
                                   :else 0))
                               )
                             (count contacts)
                             (count contacts))
          S (* (transpose A) A)
          A0 (append (- A) A)
          b0 (append b (- c b))]))

  (defmethod render :simulation [sim g]
             (.setColor g Color/black)
             (.fillRect g 0 0 (-> g .getClipBounds .getWidth) (-> g .getClipBounds .getHeight))
             (doseq [x (vals (:bodies sim))] (render x g))))
