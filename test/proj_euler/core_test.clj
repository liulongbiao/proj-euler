(ns proj-euler.core-test
  (:use clojure.test
        proj-euler.core))

(deftest p050-test
  (testing "test p050."
    (is (= [41 6] (p050 100)))
    (is (= [953 21] (p050 1000)))))
