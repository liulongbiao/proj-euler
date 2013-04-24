(ns proj-euler.helper
  (:require [clojure.math.numeric-tower :refer [lcm expt]]
            [clojure.math.combinatorics :refer [permutations combinations selections]]))

(defn divide-by?
  "whether integer n is divide by f."
  [f n]
  (zero? (mod n f)))

(defn sum [coll] (reduce + coll))

(defn prime?
  "whether integer n is prime.

  when n <= 2, then whether n is prime is up to whether n = 2."
  [n]
  (if (> n 2)
    (not-any? #(divide-by? % n) (range 2 (inc (Math/sqrt n))))
    (= n 2)))

(defn factors-of
  "find the factors of integer n, expect the 1 and itself.

  when n <= 2, then it is nil."
  [n]
  (if (> n 2)
    (let [lows (filter #(divide-by? % n) (range 2 (inc (Math/sqrt n))))
          highs (map (partial / n) lows)]
      (distinct (concat lows highs)))))

(defn natural-numbers [] (iterate inc 1))

(defn fibonacci
  "constract a fibonacci sequence."
  ([] (fibonacci 0 1))
  ([a b] (lazy-seq (cons b (fibonacci b (+ a b))))))

(defn primes [] (filter prime? (iterate inc 2)))

(defn digits-of
  "get each digit of a integer."
  [n]
  (map #(Integer/parseInt (str %)) (str n)))

(defn digits->int
  "concat a sequence of digits to constract an integer."
  [ds] (Integer/parseInt (apply str ds)))

(defn digits->bigint
  "concat a sequence of digits to constract an integer."
  [ds] (bigint (apply str ds)))

(defn palindrome?
  "whether integer n is palindrome."
  [n]
  (let [s (seq (str n))] (= s (reverse s))))

(defn factorial [n] (reduce * (range 1N (inc n))))

(defn proper-divisors
  "numbers less than n which divide evenly into n"
  [n]
  (if (> n 1) (conj (factors-of n) 1)))

(defn amicable?
  "Let d(n) be defined as the sum of proper divisors of n.

  If d(a) = b and d(b) = a, where a not= b,
  then a and b are an amicable pair and each of a and b are called amicable numbers."
  [n]
  (let [pair (sum (proper-divisors n))]
    (if (not= n pair)
      (= n (sum (proper-divisors pair))))))

(defn perfect?
  "A number n is called perfect if the sum of its proper divisors is exactly equal to itself."
  [n] (= (sum (proper-divisors n)) n))
(defn deficient?
  "A number n is called deficient if the sum of its proper divisors is less than n"
  [n] (< (sum (proper-divisors n)) n))
(defn abundant?
  "A number n is called abundant if the sum of its proper divisors exceeds n"
  [n] (> (sum (proper-divisors n)) n))

(defn permuted-integers
  "permutations of digits in n to produce integers. (remove those starts with 0)"
  [n]
  (->> n
    digits-of
    permutations
    (remove #(zero? (first %)))
    (map digits->int)))