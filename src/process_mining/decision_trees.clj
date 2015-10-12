(ns process-mining.decision-trees
  (:require [incanter.core :refer [log2]]))

(defn- partial-entropy
  [ratio]
  (if (zero? ratio)
    0
    (* ratio (log2 ratio))))

 (defn entropy [[positive population]]
  (let [pos-ratio (/ positive population)
        neg-ratio (/ (- population positive) population)]
    (-
     (+
      (partial-entropy pos-ratio)
      (partial-entropy neg-ratio)))))

(defn calculate-weighted-entropy
  [total-population]
  (fn [acc [[_ population] ent]]
    (let [weight (/ population total-population)]
      (+ acc (* weight ent)))))

(defn weighted-entropy
  "Takes a sequence of ratios and determines the weighted entropy"
  [ratio-pairs]
  (let [total-population (reduce (fn [acc [_ population]] (+ acc population)) 0 ratio-pairs)
        entropies (map (fn [pair] [pair (entropy pair)]) ratio-pairs)]
    (reduce (calculate-weighted-entropy total-population)
            0 entropies)))

(defn information-gain
  "Takes two entropies and calculates information gain."
  [e1 e2]
  (- e1 e2))
