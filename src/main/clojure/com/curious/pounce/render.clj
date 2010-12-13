(ns com.curious.pounce.render
  "Defines the render multi function.")

(defmulti render
  "Multi function for rendering, dispatches on :type in metadata."
  :type)

