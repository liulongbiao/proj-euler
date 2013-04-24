(ns proj-euler.problems.prob-021-030
  (:require [proj-euler.helper :refer [sum amicable? abundant?]]))

(defn p021
  ([] (p021 10000))
  ([n] (sum (filter amicable? (range n)))))

(defn p023
  []
  (let [abundants (apply sorted-set (filter abundant? (range 1 28134)))
        sum-of-abundants? (fn [n] (some #(abundants (- n %))
                                    (take-while #(< % n) abundants)))]
    (sum (remove sum-of-abundants? (range 1 28134)))))