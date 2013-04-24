(ns proj-euler.core-test
  (:use clojure.test
        proj-euler.core))

(deftest p018-test
  (testing "test p018."
    (is (= [23] (p018 [[3]
                     [7 4]
                     [2 4 6]
                     [8 5 9 3]])))))

(deftest p050-test
  (testing "test p050."
    (is (= [41 6] (p050 100)))
    (is (= [953 21] (p050 1000)))))
