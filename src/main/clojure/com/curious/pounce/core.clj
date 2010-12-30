(ns com.curious.pounce.core
  (:import javax.swing.JFrame
           javax.swing.JPanel
           java.awt.event.ActionListener
           javax.swing.Timer)
  (:refer-clojure :exclude [+ - * / < <= > >= = not= max-key min-key])
  (:use com.curious.pounce.body
        com.curious.pounce.shape
        com.curious.pounce.simulation
        com.curious.pounce.math.matrix
        com.curious.pounce.math.math
        com.curious.pounce.render))

(def sim (atom (simulation {:body (assoc (body (transform 225 50 0) (polygon 1 [0 0] [50 0] [50 50] [0 50]))
                                    :angular-momentum 0)
                            :ground (body identity-transform (polygon [0 0] [500 0] [500 10] [0 10]))})))

(def panel (proxy [JPanel] [] (paintComponent [g]
                                              (render @sim g))))
(def paint-listener (proxy [ActionListener] []
                      (actionPerformed [_]
                                       (.repaint panel))))
(def simulation-listener (proxy [ActionListener] []
                           (actionPerformed [_]
                                            
                                              
                                            (action @sim :body {:force (matrix 0 0) :torque 1000})
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
