(ns aoc2022.02-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.02 :refer [decode-opponent-shape
                                round-score
                                solution-1
                                solution-2]]))

(deftest decode-opponent-shape-test
  (is (= :rock (decode-opponent-shape "A")))
  (is (= :paper (decode-opponent-shape "B")))
  (is (= :scissors (decode-opponent-shape "C"))))
  
(deftest score-test
  (let [winning-score 6
        losing-score 0
        draw-score 3]
    (testing "winning combinations"
      (is (= losing-score (round-score :rock :scissors)))
      (is (= losing-score (round-score :paper :rock)))
      (is (= losing-score (round-score :scissors :paper))))
    
    (testing "draw combinations"
      (is (= draw-score (round-score :rock :rock)))
      (is (= draw-score (round-score :paper :paper)))
      (is (= draw-score (round-score :scissors :scissors))))
    
    (testing "losing combinations"
      (is (= winning-score (round-score :rock :paper)))
      (is (= winning-score (round-score :paper :scissors)))
      (is (= winning-score (round-score :scissors :rock))))))

(deftest solution-1-test []
  (is (= 15 (solution-1 (slurp "resources/02-sample-input.txt")))))

(deftest solution-2-test []
  (is (= 12 (solution-2 (slurp "resources/02-sample-input.txt")))))
