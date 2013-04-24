(ns proj-euler.helper
  (:require [clojure.math.numeric-tower :refer [lcm expt]]
            [clojure.math.combinatorics :refer [permutations combinations selections]]))

(defn devide-by?
  "whether integer n is divide by f."
  [f n]
  (zero? (mod n f)))

(defn sum [coll] (reduce + coll))

(defn prime?
  "whether integer n is prime.

  when n <= 2, then whether n is prime is up to whether n = 2."
  [n]
  (if (> n 2)
    (not-any? #(devide-by? % n) (range 2 (inc (Math/sqrt n))))
    (= n 2)))

(defn factors-of
  "find the factors of integer n, expect the 1 and itself.

  when n <= 2, then it is nil."
  [n]
  (if (> n 2)
    (let [lows (filter #(devide-by? % n) (range 2 (inc (Math/sqrt n))))
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