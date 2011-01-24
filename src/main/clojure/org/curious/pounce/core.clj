(ns org.curious.pounce.core
  (:import javax.swing.JFrame
           javax.swing.JPanel
           java.awt.event.ActionListener
           javax.swing.Timer
           java.awt.event.MouseAdapter
           java.awt.Color)
  (:use org.curious.pounce.body
        org.curious.pounce.shape
        org.curious.pounce.simulation
        org.curious.pounce.math.matrix
        org.curious.pounce.math.core
        org.curious.pounce.render
        org.curious.pounce.collision))

(def box (simulation/new-id))
(def ground (simulation/new-id))
(def sim (atom (doto (simulation/create)
                 (simulation/assoc box (assoc (body/create (transform 225 200 0) (polygon 1 [0 0] [50 0] [50 50] [0 50]))
                                         :angular-momentum 0))
                 (simulation/assoc ground (body/create identity-transform (polygon [0 0] [500 0] [500 10] [0 10]))))))
(comment (def c (atom nil))
         (defmethod process-contact #{:body :ground} [contact]
                    (swap! c (fn [x] (conj x contact)))
                    (println contact)
                    (let [b (get-body @sim (:body1 contact))]
                      (println (apply min-key y (:points (* (:transform b) (first (:shapes b)))))))))
(def paint-timer)
(def simulation-timer)
(def panel (proxy [JPanel] [] (paintComponent [g]
                                              (render @sim g)
                                              (comment
                                                (when @c
                                                  (doseq [a @c]
                                                    (.setColor g Color/yellow)
                                                    (.drawLine g (x (:point a))
                                                               (- (-> g .getClipBounds .getHeight) (y (:point a)))
                                                               (+ (x (:point a)) (* 50 (x (:normal a))))
                                                               (- (-> g .getClipBounds .getHeight) (+ (y (:point a)) (* 50 (y (:normal a))))))
                                                    (.stop paint-timer)
                                                    (.stop simulation-timer)))))))
(comment (def mouse-atom (atom nil))
         (.addMouseListener panel (proxy [MouseAdapter] []
                                    (mousePressed [e] (swap! mouse-atom (fn [x] [(.getX e) (.getY e)])))
                                    (mouseReleased [e] (println (- (.getX e) (@mouse-atom 0)) (- (@mouse-atom 1) (.getY e)))))))
(def paint-listener (proxy [ActionListener] []
                      (actionPerformed [_]
                                       (.repaint panel))))
(def simulation-listener (proxy [ActionListener] []
                           (actionPerformed [_]
;                                            (swap! c (fn [x] nil))
                                            (action @sim :body {:external-force (matrix 0 -100) :external-torque 0})
                                            (swap! sim #(simulate % 0.016)))))
(def frame (doto (JFrame. "Pounce Test")
             (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
             (.setContentPane panel)
             (.setLocation 0 0)
             (.setSize 500 500)
             (.setVisible true)))

(def paint-timer (doto (Timer. 16 paint-listener)
                   (.start)))
(def simulation-timer (doto (Timer. 16 simulation-listener)
                        (.start)))
