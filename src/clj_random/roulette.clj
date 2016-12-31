(ns clj-random.roulette
  (:require [clj-random.core :as random]))

(defn wheel
  "Return a roulette-wheel for a map of things and numerical weights.
   Input is:
     - sequence weights for each individual (used for making upper limits)
     - sequence of individuals (will become values)
   Result is a sorted map with upper-limits of ranges within [0,1] that are used
     in the probability of selection for a given value. The lower limit of the 
     first entry is 0."
  [weights individuals]
  (apply sorted-map         
         (interleave ; Make the upper-limits, these will be keys 
                     (rest 
                       (let [sum-weights (apply + weights)]
                         (reduce #(conj %1
                                        (+ (/ %2
                                              sum-weights)
                                           (last %1)))
                                 [0]
                                 weights)))
                     ; Individuals are values
                     individuals)))

(defn lspin
  "Draw from a roulette wheel."
  ([wheel]
    ; Spin if no spin value is supplied.
    (lspin wheel
               (random/lrand))); Spin value
  ([wheel spin-value]
    (let [[this-upper this-val] (first wheel)]
      (if (< spin-value this-upper)
        this-val; If the spin value is less than this upper limit, return
        (lspin (dissoc wheel (first (keys wheel))); Pop lowest value
                   spin-value)))))
    