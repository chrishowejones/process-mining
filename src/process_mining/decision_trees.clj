(ns process-mining.decision-trees
  (:require [incanter.core :refer [log2]]))

(defn- partial-entropy
  "Calculate the product of the ratio and binary log of the ratio."
  [ratio]
  (if (zero? ratio)
    0
    (* ratio (log2 ratio))))

(defn entropy
  "Given a value representing the number of positive results and the value of the overall population as a vector pair, return the value of the entropy."
  [[positive population]]
  (let [pos-ratio (/ positive population)
        neg-ratio (/ (- population positive) population)]
    (-
     (+
      (partial-entropy pos-ratio)
      (partial-entropy neg-ratio)))))

(defn calculate-weighted-entropy
  "Given a value of the total population, return a function that takes two arguments and calculates the weighted entropy. First value is the accumulated weighted entropy. The second value is a vector containing a vector pair of a value representing the number of the positive results and the total population and the entropy valuse for that pair."
  [total-population]
  (fn [acc [[_ population] ent]]
    (let [weight (/ population total-population)]
      (+ acc (* weight ent)))))

(defn weighted-entropy
  "Takes a sequence of ratios and determines the weighted entropy. Where the ratios are a vector pair consisting of the number of positive results and the total population."
  [ratio-pairs]
  (let [total-population (reduce (fn [acc [_ population]] (+ acc population)) 0 ratio-pairs)
        entropies (map (fn [pair] [pair (entropy pair)]) ratio-pairs)]
    (reduce (calculate-weighted-entropy total-population)
            0 entropies)))

(defn information-gain
  "Takes two weighted entropies and calculates information gain."
  [e1 e2]
  (- e1 e2))
