(ns clj-random.test.roulette
  (:use [clojure.test])
  (:require [clj-random.core :as random]
            [clj-random.roulette :as roulette]))

(deftest test-roulette
  (let [test-precision 0.01
      n (* 1000 (int (/ test-precision)))
      num-els 5
      target-p (/ num-els)
      els (map #(hash-map :val %) (range num-els))
      weights (repeat num-els 1)
      wheel (roulette/wheel weights els)]  
  (is
    (reduce #(and %1 %2)
            (map #(< (java.lang.Math/abs (double (- (/ % n)
                                                    target-p))) test-precision)               
                 (vals (frequencies
                                 (repeatedly n
                                             #(roulette/lspin wheel)))))))))



