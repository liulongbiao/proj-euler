(ns proj-euler.problems.prob-031-040-test
  (:use clojure.test
        proj-euler.problems.prob-031-040))

(deftest circular-prime?-test
  (testing "test circular-prime?."
    (is (circular-prime? 197))
    (is (circular-prime? 3))
    (is (circular-prime? 71))))

(deftest truncatable-prime?-test
  (testing "test truncatable-prime?."
    (is (truncatable-prime? 3797))
    (is (not (truncatable-prime? 3)))))