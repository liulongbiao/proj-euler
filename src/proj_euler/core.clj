(ns proj-euler.core
  (:require [clojure.math.numeric-tower :refer [lcm expt]]
            [clojure.math.combinatorics :refer [permutations combinations selections]]
            [proj-euler.helper :refer [sum divide-by? fibonacci prime? factors-of
                                       primes digits-of digits->int]]
            [clojure.string :refer [trim split-lines]]
            [clojure.java.io :refer [reader resource]]
            [clojure.pprint :refer [pprint]]))

(defn p041
  []
  (->> (range 9 0 -1)
    (mapcat #(permutations (range 1 (inc %))))
    (map digits->int)
    (sort >)
    (filter prime?)
    first))

(defn p047 [n]
  (letfn [(interest? [num] (= n (count (filter prime? (factors-of num)))))
          (satisfied? [coll] (every? interest? coll))]
    (first (filter satisfied? (partition n 1 (iterate inc 1))))))

;; it reverse the partition prediction, but seems use the same time as above
(defn p047-2 [n]
  (letfn [(interest? [num] (= n (count (filter prime? (factors-of num)))))
          (consecutive? [coll]
            (every? (partial = -1) (map #(apply - %) (partition 2 1 coll))))]
    (first (filter consecutive? (partition n 1 (filter interest? (iterate inc 1)))))))

(defn mid-prime-permutation [n]
  (let [prime-permutations (->> n
                             digits-of
                             permutations
                             (remove #(zero? (first %)))
                             (map digits->int)
                             (filter prime?))]
    (if (> (count prime-permutations) 2)
      (let [pair (filter #(= (reduce + %) (* 2 n))
                   (combinations prime-permutations 2))]
        (if (seq pair) (sort (cons n (first pair))))))))

(defn p049
  "Prime permutations"
  []
  (filter #(seq %) (map mid-prime-permutation (filter prime? (range 1000 9000)))))

(defn p050 [num]
  (let [max-count (count (take-while #(< % num) (reductions + (primes))))
        ps (take max-count (primes))
        consecutive-primes-sum (for [i (range max-count 0 -1)
                                     p (partition i 1 ps)
                                     :let [n (reduce + p)]
                                     :when (prime? n)]
                                  [n i])]
    (first consecutive-primes-sum)))

(defn permuted-multiple?
  [n]
  (->> n
    digits-of
    permutations
    (remove #(zero? (first %)))
    (map digits->int)))

(defn p052 []
  (first (iterate inc 1)))

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
