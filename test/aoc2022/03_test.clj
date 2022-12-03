(ns aoc2022.03-test
  (:require [clojure.test :refer [deftest is are]]
            [aoc2022.03 :refer [split-to-half-sets
                                shared-element
                                priority
                                solution-1
                                solution-2]]))

(deftest split-to-half-sets-test
  (is (= [(set ["v" "J" "r" "w" "p" "W" "t" "w" "J" "g" "W" "r"])
          (set ["h" "c" "s" "F" "M" "M" "f" "F" "F" "h" "F" "p"])]
         (split-to-half-sets "vJrwpWtwJgWrhcsFMMfFFhFp"))))
  
(deftest shared-element-test
  (is (= "p"
         (shared-element "vJrwpWtwJgWrhcsFMMfFFhFp"))))

(deftest priority-test
  (are [exp-prio c] (= exp-prio (priority c))
    1 "a"
    26 "z"
    27 "A"
    52 "Z")) 

(deftest solution-1-test
  (is (= 157 (solution-1 (slurp "resources/03-sample-input.txt")))))

(deftest solution-2-test
  (is (= 70 (solution-2 (slurp "resources/03-sample-input.txt")))))