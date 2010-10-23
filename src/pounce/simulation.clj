(ns pounce.simulation
  (:import java.awt.Color)
  (:use (pounce body math render)))

(defstruct simulation :type :bodies :gravity :actions :contact-listeners)
(defstruct action :force :torque :linear-impulse :angular-impulse :insertion)

(defn create-simulation 
  ([bodies] (create-simulation bodies (column 0 0)))
  ([bodies gravity] (create-simulation bodies gravity ()))
  ([bodies gravity listeners] (struct simulation :simulation bodies gravity (atom {}) (atom listeners))))

(defn get-body [sim at] (at (:bodies sim)))
(defn assoc-actions [sim who todo]
  (let [merge-actions
         (fn [x y]
           (if (:insertion x) 
             x
             (struct action
               (+ (:force x) (:force y))
               (+ (:torque x) (:torque y))
               (+ (:linear-impulse x) (:linear-impulse y))
               (+ (:angular-impulse x) (:angular-impulse y))
               (:insertion y))))]
    (swap! (:actions sim) #(merge-with merge-actions {who todo} %1))))

(defn apply-force [sim who magnitude] 
  (assoc-actions sim who (struct action magnitude 0 (zero 2) 0 nil)))
(defn apply-torque [sim who magnitude] 
  (assoc-actions sim who (struct action (zero 2) magnitude (zero 2) 0 nil)))
(defn apply-impulse [sim who magnitude] 
  (assoc-actions sim who (struct action (zero 2) 0 magnitude 0 nil)))
(defn assoc-body [sim at what]
  (assoc-actions sim at (struct action (zero 2) 0 (zero 2) 0 what)))
(defn conj-contact-listener [sim who] 
  ;(println "gfdgd" who)
  (swap! (:contact-listeners sim) #(conj %1 who)))

(defn apply-actions 
  ([bodies actions accum delta]
    ;(println (keys actions))
    (if (empty? bodies)
      accum
      (let [where (first (first bodies))
            action (where actions)
            who (if (:insertion action) (:insertion action) (second (first bodies)))
            linear-impulse (+ (* (if action (:force action) (zero 2)) delta) (if action (:linear-impulse action) (zero 2)))
            angular-impulse (+ (* (if action (:torque action) 0) delta) (if action (:angular-impulse action) 0))]
        ;(println where action linear-impulse angular-impulse)
        (recur 
          (rest bodies) 
          (dissoc actions where) 
          (assoc accum where (integrate-derivatives who linear-impulse angular-impulse)) delta))))
  ([bodies actions delta] 
    ;(println "Enter")
    ;(println actions)
    (apply-actions bodies actions {} delta)))

(defn step [sim delta]
  (if (> delta 0)
    (let [actions (loop [stack (keys (:bodies sim)) accum {}]
                    (if (empty? stack)
                      accum
                      (recur 
                        (rest stack) 
                        (assoc accum 
                          (first stack)
                          (if (contains? @(:actions sim) (first stack)) 
                            (assoc ((first stack) @(:actions sim)) 
                              :force 
                              (+ (:force ((first stack) @(:actions sim))) (:gravity sim)))
                            (struct action (:gravity sim) 0 (zero 2) 0 nil))))))
          new-bodies (apply-actions (:bodies sim) actions delta)]
      ;(loop [stack actions] (when-not (empty? stack) (println (first stack) (:linearMomentum ((first (first stack)) (:bodies sim)))) (recur (rest stack)))) 
      (loop [stack1 (vals new-bodies)]
        (when-not (empty? stack1)
          (loop [stack2 (rest stack1)]
            (when-not (empty? stack2)
              (let [center1 (transform (:transformation (first stack1)) (:center (first stack1)))
                    center2 (transform (:transformation (first stack2)) (:center (first stack2)))]
                (when (< (length (- center1 center2))
                         (+ (:radius (first stack1)) (:radius (last stack2))))
                  (let [contact (collision (first stack1) (first stack2) delta)]
                    (loop [stack contact]
                      (when-not (empty? stack)
                        (cond
                          (= (:category (first stack)) :EdgeEdge)
                          (loop [l @(:contact-listeners sim)] (when-not (empty? l) ((first l) contact) (recur (rest l))))
                          (= (:category (first stack)) :EdgeVertex)
                          ;(System/exit 0)
                          (loop [l @(:contact-listeners sim)] (when-not (empty? l) ((first l) contact) (recur (rest l))))
                          (= (:category (first stack)) :VertexVertex)
                          (loop [l @(:contact-listeners sim)] (when-not (empty? l) ((first l) contact) (recur (rest l)))))
                        (recur (rest stack)))))
                      ))
              (recur (rest stack2))))
          (recur (rest stack1))))
      (let [final-bodies 
            (loop [stack new-bodies accum {}] 
              (if (empty? stack) 
                accum 
                (recur (rest stack) (assoc accum (-> stack first first) (integrate-transform (-> stack first second) delta)))))]
            (create-simulation final-bodies (:gravity sim) @(:contact-listeners sim))))
    sim))

(defmethod render :simulation [sim g]
  (.setColor g Color/black)
  (.fillRect g 0 0 (-> g .getClipBounds .getWidth) (-> g .getClipBounds .getHeight))
  (loop [stack (vals (:bodies sim))] 
    (when-not (empty? stack) 
      (render (first stack) g) 
      (recur (rest stack)))))
