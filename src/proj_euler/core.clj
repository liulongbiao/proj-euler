(ns proj-euler.core
  (:require [clojure.math.numeric-tower :refer [lcm expt]]
            [clojure.math.combinatorics :refer [permutations combinations selections]]
            [proj-euler.helper :refer [sum divide-by? fibonacci prime? factors-of
                                       primes digits-of digits->int]]
            [clojure.string :refer [trim split-lines]]
            [clojure.java.io :refer [reader resource]]
            [clojure.pprint :refer [pprint]]))

(defn spiral-layer [n]
  (let [right-down (expt (inc (* 2 n)) 2)]
    (map #(- right-down (* 2 n %)) (range 3 -1 -1))))

(defn above? [[n pt]] (> (/ pt (inc (* 4 n))) 0.1))

(defn p058
  ([] (p058 0.1))
  ([percent]
    (letfn [(spiral-layer [n]
              (let [right-down (expt (inc (* 2 n)) 2)]
                (map #(- right-down (* 2 n %)) (range 3 -1 -1))))
            (layer [n] [n (count (filter prime? (spiral-layer n)))])
            (red [[_ pt] [n p]] [n (+ pt p)])
            (above? [v] (> (/ (v 1) (inc (* 4 (v 0)))) percent))
            (side-length [[n pt]] (inc (* 2 n)))]
      (->> (iterate inc 1)
        (map layer)
        (reductions red)
        (drop-while above?)
        first
        side-length))))
