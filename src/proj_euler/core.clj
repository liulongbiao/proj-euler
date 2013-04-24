(ns proj-euler.core
  (:require [clojure.math.numeric-tower :refer [lcm expt]]
            [clojure.math.combinatorics :refer [permutations combinations selections]]
            [clojure.string :refer [trim split-lines]]
            [clojure.java.io :refer [reader resource]]
            [clojure.pprint :refer [pprint]]))

(defn p016 [num]
  (sum (digits-of (expt 2 num))))

(defn roll-triangle [a b]
  (vec (for [i (range (count b))]
    (+ (b i) (max (a i) (a (inc i)))))))

(defn p018
  ([]
    (let [txt  "75
                95 64
                17 47 82
                18 35 87 10
                20 04 82 47 65
                19 01 23 75 03 34
                88 02 77 73 07 63 67
                99 65 04 28 06 16 70 92
                41 41 26 56 83 40 80 70 33
                41 48 72 33 47 32 37 16 94 29
                53 71 44 65 25 43 91 52 97 51 14
                70 11 33 28 77 73 17 78 39 68 17 57
                91 71 52 38 17 14 91 43 58 50 27 29 48
                63 66 04 68 89 53 67 30 73 16 69 87 40 31
                04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
          triangle (for [row (split-lines txt)]
                     (vec (map #(Integer. %) (.split (trim row) " "))))]
      (p018 triangle)))
  ([triangle]
    (reduce roll-triangle (reverse triangle))))

(defn divide-by?
  [f n]
  (zero? (mod n f)))

(defn leap-year?
  [year]
  (if (divide-by? 100 year)
    (divide-by? 400 year)
    (divide-by? 4 year)))

(defn sundays-from-1900
  [year]
  (quot (sum (map #(if (leap-year? %) 366 365) (range 1900 year))) 7))

;; Current not right.
(defn p019 [y1 y2]
  (- (sundays-from-1900 y2) (sundays-from-1900 y1)))

(defn factorial [n] (reduce * (range 1N (inc n))))

(defn p020
  ([] (p020 100))
  ([n] (sum (digits-of (factorial n)))))

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
  (letfn [(interest? [num] (= n (count (prime-factors-of num))))
          (satisfied? [coll] (every? interest? coll))]
    (first (filter satisfied? (partition n 1 (iterate inc 1))))))

;; it reverse the partition prediction, but seems use the same time as above
(defn p047-2 [n]
  (letfn [(interest? [num] (= n (count (prime-factors-of num))))
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

(defn p067 []
  (with-open [rdr (reader (resource "proj_euler/triangle.txt"))]
    (let [triangle (for [row (line-seq rdr)]
                     (vec (map #(Integer. %) (.split (trim row) " "))))]
      (p018 triangle))))