(ns proj-euler.problems.prob-011-020-test
  (:use clojure.test
        proj-euler.problems.prob-011-020))

(deftest p016-test
  (testing "test p016."
    (is (= 26 (p016 15)))))

(deftest p018-test
  (testing "test p018."
    (is (= [23] (p018 [[3]
                       [7 4]
                       [2 4 6]
                       [8 5 9 3]])))))

(deftest p020-test
  (testing "test p020."
    (is (= 27 (p020 10)))))