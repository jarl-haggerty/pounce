(ns com.curious.pounce.render
  "Defines the Renderable protocol.")

(defprotocol Renderable
  (render [this graphics]))
