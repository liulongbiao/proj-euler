(ns proj-euler.problems.prob-001-010
  (:require [clojure.math.numeric-tower :refer [lcm]]
            [proj-euler.helper :refer [sum divide-by? fibonacci prime? factors-of
                                       primes digits-of]]))

(defn p001
  ([] (p001 1000))
  ([n] (sum (filter #(or (divide-by? 3 %) (divide-by? 5 %)) (range 1 n)))))

(defn p002
  ([] (p002 4000000))
  ([n] (sum (filter even? (take-while #(< % n) (fibonacci))))))

(defn p003
  "find the largest prime factor"
  ([] (p003 600851475143))
  ([n] (apply max (filter prime? (factors-of n)))))

(defn palindrome? [n]
  (let [s (seq (str n))]
    (= s (reverse s))))

(defn p004 []
  (apply max (filter palindrome? (for [x (range 100 1000) y (range 100 1000)] (* x y)))))

(defn p005
  ([] (p005 20))
  ([n] (reduce lcm 1 (range 2 (inc n)))))

(defn square [n] (* n n))

(defn p006
  ([] (p006 100))
  ([n] (let [rg (range 1 (inc n))]
          (- (square (sum rg))
             (sum (map square rg))))))

(defn p007
  ([] (p007 10001))
  ([n] (first (drop (dec n) (primes)))))

(defn p008 []
  (let [n 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450]
    (apply max (map #(apply * %) (partition 5 1 (digits-of n))))))

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

(defn p010
  ([] (p010 2000000))
  ([num] (sum (take-while #(< % num) (primes)))))

;; below code use the screen method, to find primes below an integer
;; it works for small num ,but get StackOverflowError
(defn screen-primes
  [lefts]
  (if (seq lefts)
    (let [p (first lefts)]
      (cons p (screen-primes (remove #(divide-by? p %) (rest lefts)))))))

(defn primes-below [n] (screen-primes (range 2 n)))

(defn p010-1 [num]
  (sum (primes-below num)))