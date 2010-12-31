(ns com.curious.pounce.render
  "Defines the render multi function.")

(defprotocol Renderable
  (render [this graphics]))
