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
  (:refer-clojure :exclude [assoc get])
  (:import java.awt.Color
           java.awt.Graphics)
  (:require [org.curious.pounce.body :as body]
            [org.curious.pounce.math.core :as math]
            [org.curious.pounce.math.matrix :as matrix]
	    [org.curious.pounce.collision :as collision]))

(def next-id (atom 0))
(defn unique-id [] (swap! next-id inc))

(defn create
  ([] (create {}))
  ([bodies] (create bodies collision/separating-axis-collision-detection collision/velocity-based-collision-response))
  ([bodies collision-detection collision-response] {:bodies bodies :collision-detection collision-detection :collision-response collision-response}))

(defn assoc [sim id body]
  (assoc-in sim [:bodies id] (clojure.core/assoc body :id id)))
(defn add [sim body]
  (let [id (unique-id)]
    {:id id :simulation (assoc-in sim [:bodies id] (clojure.core/assoc body :id id))}))
(defn get [sim id]
  (get-in sim [:bodies id]))

(defn simulate
  ([sim delta] (simulate sim delta {}))
  ([sim delta perturbations]
     (let [bodies (into {} (filter #(not (get-in perturbations [(first %) :die])) (:bodies sim)))
	   collisions ((:collision-detection sim) (-> sim :bodies vals))
	   response ((:collision-response sim) (:bodies sim) collisions perturbations delta (:cached-collisions sim))
           perturbations+response (merge-with #(hash-map :force (matrix/add (clojure.core/get %1 :force matrix/zero)
									    (clojure.core/get %2 :force matrix/zero))
							 :torque (+ (clojure.core/get %1 :torque 0) (clojure.core/get %2 :torque 0)))
					      perturbations response)]
       (doseq [collision collisions]
	 (collision/process-collision collision))
       (clojure.core/assoc sim :bodies (into {} (map #(vector (first %)

                                                              (body/update (second %) delta
                                                                           (get-in perturbations+response [(first %) :force] matrix/zero)
                                                                           (get-in perturbations+response [(first %) :torque] 0)
                                                                           (get-in response [(first %) :velocity] matrix/zero)))
                                                     bodies))
                           :cached-collisions collisions))))

(defn render [sim graphics]
  (.setColor graphics Color/green)
  (doseq [x (vals (:bodies sim))]
    (body/render x graphics)))
