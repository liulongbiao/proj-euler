(ns proj-euler.problems.prob-061-070
  (:require [clojure.java.io :refer [reader resource]]
            [clojure.string :refer [trim split-lines]]
            [proj-euler.problems.prob-011-020 :refer [p018]]
            [proj-euler.helper :refer [cube cube? permuted-integers]]))

(defn cubic-permutations? [i n]
  (= n (count (filter cube? (permuted-integers (cube i))))))

(defn p062
  ([] (p062 5))
  ([n]
    (first (filter #(cubic-permutations? % n) (iterate inc 1)))))

(defn p067 []
  (with-open [rdr (reader (resource "proj_euler/triangle.txt"))]
    (let [triangle (for [row (line-seq rdr)]
                     (vec (map #(Integer. %) (.split (trim row) " "))))]
      (p018 triangle))))