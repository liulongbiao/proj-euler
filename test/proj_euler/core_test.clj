(ns proj-euler.core-test
  (:use clojure.test
        proj-euler.core))

(deftest p001-test
  (testing "test p001."
    (is (= 23 (p001 10)))))
