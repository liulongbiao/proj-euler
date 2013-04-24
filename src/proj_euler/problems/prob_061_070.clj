(ns proj-euler.problems.prob-061-070
  (:require [clojure.java.io :refer [reader resource]]
            [proj-euler.problems.prob-011-020 :refer [p018]]))

(defn p067 []
  (with-open [rdr (reader (resource "proj_euler/triangle.txt"))]
    (let [triangle (for [row (line-seq rdr)]
                     (vec (map #(Integer. %) (.split (trim row) " "))))]
      (p018 triangle))))