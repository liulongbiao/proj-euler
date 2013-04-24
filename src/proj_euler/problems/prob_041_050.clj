(ns proj-euler.problems.prob-041-050
  (:require [clojure.math.combinatorics :refer [permutations combinations]]
            [proj-euler.helper :refer [digits-of digits->int prime? primes sum
                                       factors-of permuted-integers]]))

(defn p041
  []
  (->> (range 9 0 -1)
    (mapcat #(permutations (range 1 (inc %))))
    (map digits->int)
    (sort >)
    (filter prime?)
    first))

(defn p047
  ([] (p047 4))
  ([n]
    (letfn [(interest? [num] (= n (count (filter prime? (factors-of num)))))
            (satisfied? [coll] (every? interest? coll))]
      (->> (iterate inc 1)
        (partition n 1)
        (filter satisfied?)
        first
        first))))

;; it reverse the partition prediction, but seems use the same time as above
(defn p047-2 [n]
  (letfn [(interest? [num] (= n (count (filter prime? (factors-of num)))))
          (consecutive? [coll]
            (every? (partial = -1) (map #(apply - %) (partition 2 1 coll))))]
    (first (filter consecutive? (partition n 1 (filter interest? (iterate inc 1)))))))

(defn mid-prime-permutation [n]
  (let [prime-permutations (filter prime? (permuted-integers n))]
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