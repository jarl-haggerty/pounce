(ns org.curious.pounce.core
  (:import javax.swing.JFrame
           javax.swing.JPanel
           java.awt.event.ActionListener
           javax.swing.Timer
           java.awt.event.MouseAdapter
           java.awt.Color
           java.awt.Graphics)
  (:require [org.curious.pounce.body :as body]
	    [org.curious.pounce.shape :as shape]
	    [org.curious.pounce.simulation :as simulation]
	    [org.curious.pounce.math.matrix :as matrix]
	    [org.curious.pounce.math.core :as math]
	    [org.curious.pounce.collision :as collision]))

(set! *warn-on-reflection* true)

(def box (simulation/unique-id))
(def box2 (simulation/unique-id))
(def ground (simulation/unique-id))
(def sim (atom (-> (simulation/create)
		   (simulation/assoc box (body/create (matrix/transformation 265 200 0) (shape/polygon 1 [0 0] [50 0] [50 50] [0 50])))
		   (simulation/assoc box2 (body/create (matrix/transformation 225 140 0) (shape/polygon 1 [0 0] [50 0] [50 50] [0 50])))
		   (simulation/assoc ground (body/create (matrix/transformation 0 120 0) (shape/polygon [0 0] [500 0] [500 10] [0 10]))))))

(comment (defmethod collision/process-collision #{box2 ground} [contact]
		    (println "Box2 hit the ground!"))

	 (defmethod collision/process-collision #{box box2} [contact]
		    (println "Box1 hit box2!")))

(def last-time (atom nil))
(def elapsed-time (atom 0))
(def fps (atom 1))

(def paint-timer)
(def simulation-timer)
(def ^JPanel panel (doto (proxy [JPanel] [] (paintComponent [^Graphics g]
                                                            (.setColor g Color/black)
                                                            (.fillRect g 0 0 (-> g .getClipBounds .getWidth) (-> g .getClipBounds .getHeight))
                                                            (.setColor g Color/green)
                                                            (.drawString g (str "FPS: " @fps) 0 20)
                                                            (.drawString g (str "Elapsed Time: " @elapsed-time) 0 40)
                                                            (simulation/render @sim g)
                                                            ))))

(def paint-listener (proxy [ActionListener] []
                      (actionPerformed [_]
                                       (.repaint panel))))
(def simulation-listener (proxy [ActionListener] []
                           (actionPerformed [_]
                                            (let [current-time (System/currentTimeMillis)]
                                              (when @last-time
                                                (swap! fps (fn [x] (/ 1000.0 (- current-time @last-time))))
                                                (swap! elapsed-time #(+ % 0.016))
                                                (swap! sim #(simulation/simulate % 0.016 {box {:force (matrix/column 0 -100)} box2 {:force (matrix/column 0 -100)}})))
                                              (swap! last-time (fn [x] current-time)))
					    )))
(def frame (doto (JFrame. "Pounce Test")
             (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
             (.setContentPane panel)
             (.setLocation 0 0)
             (.setSize 500 500)
             (.setVisible true)))

(swap! last-time (fn [x] (System/currentTimeMillis)))
(def paint-timer (doto (Timer. 16 paint-listener)
                   (.start)))
(def simulation-timer (doto (Timer. 16 simulation-listener)
                        (.start)))
