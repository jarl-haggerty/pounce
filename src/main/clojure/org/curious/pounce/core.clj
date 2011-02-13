(ns org.curious.pounce.core
  (:import javax.swing.JFrame
           javax.swing.JPanel
           java.awt.event.ActionListener
           javax.swing.Timer
           java.awt.event.MouseAdapter
           java.awt.Color)
  (:require [org.curious.pounce.body :as body]
	    [org.curious.pounce.shape :as shape]
	    [org.curious.pounce.simulation :as simulation]
	    [org.curious.pounce.math.matrix :as matrix]
	    [org.curious.pounce.math.core :as math]
	    [org.curious.pounce.collision :as collision]))

(def box (simulation/unique-id))
(def box2 (simulation/unique-id))
(def ground (simulation/unique-id))
(def sim (atom (-> (simulation/create)
		   (simulation/assoc box (body/create (matrix/transformation 225 200 0) (shape/polygon 1 [0 0] [50 0] [50 50] [0 50])))
		   (simulation/assoc box2 (body/create (matrix/transformation 225 140 0) (shape/polygon 1 [0 0] [50 0] [50 50] [0 50])))
		   (simulation/assoc ground (body/create matrix/identity-transform (shape/polygon [0 0] [500 0] [500 10] [0 10]))))))

;(println @sim)
;(System/exit 0)

(comment (defmethod collision/process-collision #{box2 ground} [contact]
		    (println "Box2 hit the ground!"))

	 (defmethod collision/process-collision #{box box2} [contact]
		    (println "Box1 hit box2!")))

(def paint-timer)
(def simulation-timer)
(def panel (proxy [JPanel] [] (paintComponent [g]
                                              ;(simulation/render @sim g)
					      )))

(def last-time (atom (System/currentTimeMillis)))

(def paint-listener (proxy [ActionListener] []
                      (actionPerformed [_]
                                       (.repaint panel))))
(def simulation-listener (proxy [ActionListener] []
                           (actionPerformed [_]
					    (println (- (System/currentTimeMillis) @last-time))
					    (swap! last-time (fn [x] (System/currentTimeMillis)))
					    (swap! sim #(simulation/simulate % 0.016 {box {:force (matrix/column 0 -1000)} box2 {:force (matrix/column 0 -500)}}))
					    )))
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
