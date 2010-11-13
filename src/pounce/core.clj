(ns pounce.core
  (:import javax.swing.JFrame
           javax.swing.JComponent
           javax.swing.Timer
           java.awt.Dimension
           java.awt.Graphics2D
           java.awt.Point
           java.awt.Color
           java.awt.event.ActionListener)
  (:use pounce.math.math
        pounce.simulation
        pounce.body
        pounce.shape
        pounce.render))
(comment
(set! *warn-on-reflection* true)

(def object1 
  (create-body 
    (create-transformation (column 225 400))
    (create-shape 1 (column 0 0) (column 50 0) (column 50 50) (column 0 50))))
(def object2 
  (create-body 
    (create-transformation (column 225 400))
    (create-shape 1 (column 0 0) (column 50 0) (column 50 50) (column 0 50))))
(def ground 
  (create-body 
    (create-transformation (column 0 100))
    (create-shape positive-infinity (column 100 10) (column 400 10) (column 400 20) (column 100 20))))

(def running (atom true))
(def sim 
  (atom (create-simulation
          {:object1 object1 :ground ground})))

(def frame-count (atom 0))
(def fps (atom 0))
(def loop-time (atom 0))
(def last-time (atom -1))
(def elapsed-time (atom 0))
(def ^JComponent contents (proxy [JComponent] []
                (paintComponent [^Graphics2D g]
                  (render @sim g))))

(def ^ActionListener painter (proxy [ActionListener] []
                               (actionPerformed [e]
                                 (.repaint contents))))
(def ^ActionListener updater (proxy [ActionListener] []
                               (actionPerformed [e]
                                 (let [loop-time (System/currentTimeMillis)]
                                   (when (< @last-time 0)
                                     (swap! last-time #(do %1 loop-time)))
                                   (swap! elapsed-time #(+ %1 (- loop-time @last-time)))
                                   (apply-torque @sim :object1 0.1)
                                   (swap! sim #(step %1 0.017))
                                   (when (> @elapsed-time 1000)
                                     (swap! fps #(do %1 @frame-count))
                                     (swap! frame-count #(do %1 0))
                                     (swap! elapsed-time #(do %1 0)))
                                   (swap! last-time #(do %1 loop-time))
                                   (swap! frame-count inc)))))
(def component-listener (proxy [ComponentListener] []
                          (componentHidden [_] (swap! running (fn [x] false)))
                          (componentMoved [_] )
                          (componentResized [_] )
                          (componentShown [_] )))

(defn run []
  (doto (JFrame. "Pounce")
    (.add contents)
    (.addComponentListener component-listener)
    (.setSize 500 500)
    (.setLocation 600 300)
    (.setVisible true))
  (swap! frame-count (fn [x] 0) 0)
  (swap! fps (fn [x] 0) 0)
  (swap! loop-time (fn [x] 0) 0)
  (swap! last-time (fn [x] -1) -1)
  (swap! elapsed-time (fn [x] 0) 0)
  (let [paint-timer (Timer. 17 painter)
        update-timer (Timer. 17 updater)]
    (swap! running (fn [x] true))
    (.start paint-timer)
    (.start update-timer)
    (while @running)
    (.stop paint-timer)
    (.stop update-timer)))


)

