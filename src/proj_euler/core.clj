(ns proj-euler.core
  (:require [clojure.math.numeric-tower :refer [lcm expt]]
            [clojure.math.combinatorics :refer [permutations combinations selections]]
            [proj-euler.helper :refer [sum divide-by? fibonacci prime? factors-of
                                       primes digits-of digits->int]]
            [clojure.string :refer [trim split-lines]]
            [clojure.java.io :refer [reader resource]]
            [clojure.pprint :refer [pprint]]))

(defn proper-divisors [n]
  (if (> n 1)
    (conj (factors-of n) 1)))
(defn amicable? [n]
  (let [pair (sum (proper-divisors n))]
    (if (not= n pair)
      (= n (sum (proper-divisors pair))))))
(defn p021
  ([] (p021 10000))
  ([n] (sum (filter amicable? (range n)))))

(defn perfect? [n] (= (sum (proper-divisors n)) n))
(defn deficient? [n] (< (sum (proper-divisors n)) n))
(defn abundant? [n] (> (sum (proper-divisors n)) n))

(defn p023
  []
  (let [abundants (apply sorted-set (filter abundant? (range 1 28134)))
        sum-of-abundants? (fn [n] (some #(abundants (- n %))
                                        (take-while #(< % n) abundants)))]
    (sum (remove sum-of-abundants? (range 1 28134)))))

(defn rotates
  [ds]
  (let [c (count ds)]
    (take c (partition c 1 (cycle ds)))))

(defn circular-prime?
  [n]
  (->> n
    digits-of
    rotates
    (map digits->int)
    (every? prime?)))

(defn p035
  ([] (p035 1000000))
  ([n]
    (count (filter circular-prime? (take-while #(< % n) (primes))))))

(defn truncatable-prime?
  [n]
  (if (prime? n)
    (let [ds (vec (digits-of n))
          ct (count ds)]
      (if (> ct 1)
        (->> (range 1 ct)
          (mapcat #(vector (take % ds) (take-last % ds)) )
          (map digits->int)
          (every? prime?))))))

(defn p037
  []
  (sum (take 11 (filter truncatable-prime? (primes)))))

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
