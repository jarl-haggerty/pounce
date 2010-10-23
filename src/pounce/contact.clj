(ns pounce.contact)

(defrecord contact [category time-point points])

(defn create-contact [category time-point & more] (contact. category time-point `(~@more)))

(def a (create-contact :None 100))