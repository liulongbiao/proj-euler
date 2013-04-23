(ns proj-euler.core
  (:require [clojure.math.numeric-tower :refer [lcm expt]]
            [clojure.math.combinatorics :refer [permutations combinations]]
            [clojure.string :refer [trim split-lines]]
            [clojure.pprint :refer [pprint]]))

(defn factor-of?
  [n f]
  (zero? (mod n f)))

(defn p001
  ([] (p001 1000))
  ([n] (reduce + (filter #(or (factor-of? % 3) (factor-of? % 5)) (range 1 n)))))

(defn fib
  ([] (fib 0 1))
  ([a b] (lazy-seq (cons b (fib b (+ a b))))))

(defn p002
  []
  (reduce + (filter even? (take-while (partial > 4000000) (fib)))))

(defn prime?
  [n]
  (or (= n 2)
    (not-any? (partial factor-of? n) (range 2 (inc (Math/sqrt n))))))

(defn factors-of [num]
  (let [lows (filter (partial factor-of? num) (range 2 (inc (Math/sqrt num))))
        highs (map (partial / num) lows)]
    (distinct (concat lows highs))))

(defn prime-factors-of
  [num]
  (filter prime? (factors-of num)))

(defn p003
  "find the largest prime factor"
  [num]
  (apply max (prime-factors-of num)))

(defn palindrome?
  [num]
  (let [s (seq (str num))]
    (= s (reverse s))))

(defn p004 []
  (apply max (filter palindrome? (for [x (range 100 1000) y (range 100 1000)] (* x y)))))

(defn p005
  ([] (p005 20))
  ([n] (reduce lcm 1 (range 2 (inc n)))))

(defn square [n] (* n n))

(defn p006
  [n]
  (let [rg (range 1 (inc n))]
    (- (square (reduce + rg))
      (reduce + (map square rg)))))

(defn primes [] (filter prime? (iterate inc 2)))

(defn p007
  [n]
  (first (drop (dec n) (primes))))

(defn digits-of [n]
  (map #(Integer/parseInt (str %)) (str n)))

(defn p008 []
  (let [n 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450]
    (apply max (map
                 #(apply * %) (partition 5 1 (digits-of n))))))

(defn pythagoreans [num]
  (for [x (range 1 (inc (/ num 3)))
        y (range x (inc (/ num 2)))
        :let [z (- num x y)]
        :when (and (<= y z)
                (=  (square z)
                  (+ (square x) (square y))))]
    [x y z]))

(defn p009 [num]
  (let [ps (pythagoreans num)]
    (if (seq ps)
      (let [[x y z] (first ps)]
        (* x y z)))))

(defn p010 [num]
  (reduce + (take-while #(< % num) (primes))))

;; below code use the screen method, to find primes below an integer
;; it works for small num ,but get StackOverflowError
(defn screen-primes
  [lefts]
  (if (seq lefts)
    (let [p (first lefts)]
      (cons p (screen-primes (remove #(factor-of? % p) (rest lefts)))))))

(defn primes-below [n] (screen-primes (range 2 n)))

(defn p010-1 [num]
  (reduce + (primes-below num)))

(defn p016 [num]
  (reduce + (digits-of (expt 2 num))))

(defn roll-trangle [a b]
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
  ([trangle]
    (reduce roll-trangle (reverse trangle))))

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
  (quot (reduce + (map #(if (leap-year? %) 366 365) (range 1900 year))) 7))

;; Current not right.
(defn p019 [y1 y2]
  (- (sundays-from-1900 y2) (sundays-from-1900 y1)))

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
                             (map #(Integer/parseInt (apply str %)))
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