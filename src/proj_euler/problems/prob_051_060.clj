(ns proj-euler.problems.prob-051-060
  (:require [clojure.math.numeric-tower :refer [expt]]
            [proj-euler.helper :refer [prime? factorial]]))

(defn choose [n r]
  (/ (factorial n) (* (factorial r) (factorial (- n r)))))

(defn p053 []
  (count (for [n (range 1 101)
               r (range 1 (inc n))
               :let [c (choose n r)]
               :when (> c 1000000)]
           c)))

(defn p058
  ([] (p058 0.1))
  ([percent]
    (letfn [(spiral-layer [n]
              (let [right-down (expt (inc (* 2 n)) 2)]
                (map #(- right-down (* 2 n %)) (range 3 -1 -1))))
            (layer [n] [n (count (filter prime? (spiral-layer n)))])
            (red [[_ pt] [n p]] [n (+ pt p)])
            (above? [[n pt]] (> (/ pt (inc (* 4 n))) percent))
            (side-length [[n pt]] (inc (* 2 n)))]
      (->> (iterate inc 1)
        (map layer)
        (reductions red)
        (drop-while above?)
        first
        side-length))))