(ns proj-euler.problems.prob-031-040
  (:require [proj-euler.helper :refer [digits-of digits->int prime? primes sum]]))

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