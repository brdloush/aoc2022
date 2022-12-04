(ns aoc2022.04-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.04 :refer [sort-intervals
                                shared-interval
                                parse-line
                                fully-contained
                                solution-1
                                solution-2]]))

(deftest sort-intervals-test
  (testing "not contained"
    (is (= [[1 2] [3 4]]
           (sort-intervals [1 2] [3 4])))
    (is (= [[1 2] [3 4]]
           (sort-intervals [3 4] [1 2]))))
  
  (testing "touching by 1 element"
    (is (= [[1 2] [2 3]]
           (sort-intervals [1 2] [2 3])))
    (is (= [[1 2] [2 3]]
           (sort-intervals [2 3] [1 2]))))
  
  (testing "fully included"
    (is (= [[1 2] [1 3]]
           (sort-intervals [1 2] [1 3])))
    (is (= [[1 2] [1 3]]
           (sort-intervals [1 3] [1 2]))))
  
  (testing "same"
    (is (= [[1 2] [1 2]]
           (sort-intervals [1 2] [1 2])))))

(deftest shared-interval-test
  (testing "not contained"
    (is (= nil
           (shared-interval [1 2] [3 4]))))

  (testing "touching by 1 element"
    (is (= [2 2]
           (shared-interval [1 2] [2 3])))
    (is (= [2 2]
           (shared-interval [2 3] [1 2]))))

  (testing "fully included"
    (is (= [2 9]
           (shared-interval [1 9] [2 10])))
    (is (= [2 9]
           (shared-interval [2 10] [1 9]))))

  (testing "same"
    (is (= [1 5]
           (shared-interval [1 5] [1 5]))))
  
  (testing "first interval fully containing second"
    (is (= [2 4]
           (shared-interval [1 5] [2 4])))))

(deftest fully-contained-test
  (testing "not contained"
    (is (= false
           (fully-contained [1 2] [3 4])))))

(deftest parse-line-test
  (is (= [[2 4] [6 8]]
         (parse-line "2-4,6-8"))))

(deftest solution-1-test
  (is (= 2
         (solution-1 (slurp "resources/04-sample-input.txt")))))

(deftest solution-2-test
  (is (= 4
         (solution-2 (slurp "resources/04-sample-input.txt")))))