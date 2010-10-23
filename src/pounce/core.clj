(ns pounce.core
  (:import 
    (javax.swing JFrame JComponent Timer)
    (java.awt Dimension Graphics2D Point Color)
    (java.awt.event ActionListener))
  (:use (pounce math simulation render body shape))
  (:gen-class))

(set! *warn-on-reflection* true)

;(def A (matrix [-1 1 2 -1 3 1] 3 2))
;(def b (matrix [2 -1 3]))
;(def c (matrix [1 1]))
;(def M (stack (append (zero 2 2) (transpose A)) (append (- A) (zero 3 3))))
;(def q (stack (- c) b))

;(println M)
;(println q)
;(println (solve-lcp M q))

(def x (polynomial (term 6 'x 3 'y 2) (term 20 '(y 1) 3) (term 1)))
(def y (polynomial (term 6 'x 3 'y 2) (term 20 'x 3 'y 3) (term 1)))
;(println (map #(with-meta % {}) x))
(println x)
(println y)
(println (+ x y))

(System/exit 0)

(def object1 
  (create-body 
    (create-transformation (column 225 100))
    (create-shape 1 (column 0 0) (column 50 0) (column 50 50) (column 0 50))))
(def object2 
  (create-body 
    (create-transformation (column 225 400))
    (create-shape 1 (column 0 0) (column 50 0) (column 50 50) (column 0 50))))
(def ground 
  (create-body 
    (create-transformation (column 0 200))
    (create-shape positive-infinity (column 100 10) (column 400 10) (column 400 20) (column 100 20))))

(def contacts (atom ()))
(def types (atom ()))
(def running (atom true))
(def sim 
  (atom (create-simulation
          {:object1 object1 :object2 object2 :ground ground})))
(conj-contact-listener @sim 
  (fn [x] 
    (println x)
    (swap! running #(do %1 false))
    (swap! contacts #(apply conj %1 (flatten (map :points x))))
    (swap! types #(do %1 (print-str (flatten  x))))
    ;(apply-impulse @sim :object1 (create-vec2 0 -100))
    ))
    ;(swap! contacts #(apply conj %1 (-> x first :points)))))
;(println (:contact-listeners @sim))

(def frame-count (atom 0))
(def fps (atom 0))
(def loop-time (atom 0))
(def last-time (atom -1))
(def elapsed-time (atom 0))
(def ^JFrame frame (JFrame. "Test"))
(def ^JComponent contents (proxy [JComponent] []
                (paintComponent [^Graphics2D g]
                  (render @sim g)
                  (.drawString g (String/valueOf @fps) 0 10)
                  (.drawString g (String/valueOf @types) 0 100)
                  ;(.drawString g (String/valueOf (:rotation (:transformation (get-body @sim :object2)))) 0 200)
                  (.setColor g Color/yellow)
                  (loop [stack @contacts]
                    (when-not (empty? stack)
                      (.drawLine g (- (:x (first stack)) 50) (- (-> g .getClipBounds .getHeight) (:y (first stack))) (+ (:x (first stack)) 50) (- (-> g .getClipBounds .getHeight)(:y (first stack))))
                      (.drawLine g (:x (first stack)) (- (-> g .getClipBounds .getHeight) (- (:y (first stack)) 50)) (:x (first stack)) (- (-> g .getClipBounds .getHeight) (+ (:y (first stack)) 50)))
                      (recur (rest stack))))
                  ;(swap! contacts #(do %1 ()))
                  (swap! types #(do %1 ())))))

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
                                   ;(apply-torque @sim :object2 1)
                                   ;(apply-force @sim :object1 (create-vec2 0 2))
                                   (apply-force @sim :object2 (create-vec2 0 -50))
                                   (apply-force @sim :ground (create-vec2 0 -500))
                                   (swap! sim #(step %1 0.017))
                                   (when (not @running) (.stop (.getSource e)))
                                   ;(when (not @running) (println "gfds") (apply-impulse @sim :object2 (* -2 (-> @sim (get-body :object2) :linearMomentum))) 
                                   ;  (swap! running #(do %1 true))
                                   ;  )
                                   (get-body @sim :object1)
                                   (get-body @sim :object2)
                                   
                                   (when (> @elapsed-time 1000)
                                     (swap! fps #(do %1 @frame-count))
                                     (swap! frame-count #(do %1 0))
                                     (swap! elapsed-time #(do %1 0)))
                                   (swap! last-time #(do %1 loop-time))
                                   (swap! frame-count inc)))))

(defn -main [& args]
  (.add frame contents)
  (.setSize frame 500 500)
  (.setLocation frame 600 300)
  (.setVisible frame true)
  (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
  (.start (Timer. 17 painter))
  (.start (Timer. 17 updater)))
(-main)




