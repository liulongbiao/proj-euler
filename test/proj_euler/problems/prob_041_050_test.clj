(ns proj-euler.problems.prob-041-050-test
  (:use clojure.test
        proj-euler.problems.prob-041-050))

(deftest p047-test
  (testing "test p047."
    (is (= 14 (p047 2)))
    (is (= 644 (p047 3)))))

(deftest p050-test
  (testing "test p050."
    (is (= [41 6] (p050 100)))
    (is (= [953 21] (p050 1000)))))