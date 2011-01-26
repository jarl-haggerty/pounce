(comment
  Copyright 2010 Jarl Haggerty

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0
  
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.)

(ns org.curious.pounce.simulation
  (:refer-clojure :exclude [assoc])
  (:import java.awt.Color
           java.awt.Graphics)
  (:require [org.curious.pounce.body :as body]
            [org.curious.pounce.math.core :as math]
            [org.curious.pounce.math.matrix :as matrix]
            [org.curious.pounce.render :as render]))

(def next-id (atom -1))
(defn get-id [] (swap! next-id inc))

(defrecord Simulation [bodies collision-detection collision-response]
  render/Renderable
  (render [this graphics]
          (.setColor graphics Color/black)
          (.fillRect graphics 0 0 (-> graphics .getClipBounds .getWidth) (-> graphics .getClipBounds .getHeight))
          (doseq [x (vals (:bodies sim))]
            (render x graphics))))

(defn simulation
  [bodies collision-detection collision-response] (Simulation. bodies collision-detection collision-response))

(defn assoc [sim id body]
  (assoc-in sim :bodies id body))
(defn add [sim body]
  (let [id (get-id)]
    {:id id :simulation (assoc sim id body)}))

(defn simulate
  ([sim delta] (simulate sim delta {}))
  ([sim delta perturbations]
     (let [merge-function (fn [x y] (merge-with #(cond (number? %1) (+ %1 %2)
                                                      (matrix/matrix? %1) (matrix/add %1 %2)
                                                      :else %1)
                                               x y))
           bodies (into {} (filter #(-> perturbations (get-in (first %) :die) not) (:bodies sim)))
           new-bodies (if-let [collisions ((:collision-detection sim) (vals bodies))]
                        ((:collision-response sim) bodies collisions perturbations delta))]
       (asoc sim :bodies new-bodies))))
