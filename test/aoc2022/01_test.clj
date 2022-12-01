(ns aoc2022.01-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.01 :refer [sorted-sums solution-part-1 solution-part-2]]))

(deftest sorted-sums-test
  (let [oficial-sample-input (slurp "resources/01-sample-input.txt")]
    (testing "simple sorted sums"
      (is (= '(15 7 6)
             (sorted-sums "1
2
3

4
5
6

7"))))
    
    (testing "official solution for part 1"
      (is (= 24000
             (solution-part-1 oficial-sample-input))))

    (testing "official solution for part 2"
      (is (= 45000
             (solution-part-2 oficial-sample-input))))))
