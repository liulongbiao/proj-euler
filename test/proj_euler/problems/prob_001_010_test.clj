(ns proj-euler.problems.prob-001-010-test
  (:use clojure.test
        proj-euler.problems.prob-001-010))

(deftest p001-test
  (testing "test p001."
    (is (= 23 (p001 10)))))

(deftest p002-test
  (testing "test p002."
    (is (= 10 (p002 10)))))

(deftest p003-test
  (testing "test p003."
    (is (= 29 (p003 13195)))))

(deftest p005-test
  (testing "test p005."
    (is (= 2520 (p005 10)))))

(deftest p006-test
  (testing "test p006."
    (is (= 2640 (p006 10)))))

(deftest p007-test
  (testing "test p007."
    (is (= 13 (p007 6)))))

(deftest p010-test
  (testing "test p010."
    (is (= 17 (p010 10)))))