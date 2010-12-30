(ns com.curious.pounce.collision)

(defn detect-overlap [body1 body2]
  )

(defn predict-collision []
  "Calculates all the points of collisions that will occur in the next delta seconds between two bodies, neglecting rotation."
  )

(defmulti process-contact #(hash-set (:body1 %) (:body2 %)))
