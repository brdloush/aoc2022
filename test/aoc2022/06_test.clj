(ns aoc2022.06-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.06 :refer [first-marker-after-char]]))

(deftest first-marker-after-char-test
  (testing "searching in groups of 4"
    (is (= 7 (first-marker-after-char "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 4)))
    (is (= 5 (first-marker-after-char "bvwbjplbgvbhsrlpgdmjqwftvncz" 4)))
    (is (= 6 (first-marker-after-char "nppdvjthqldpwncqszvftbrmjlhg" 4)))
    (is (= 10 (first-marker-after-char "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 4)))
    (is (= 11 (first-marker-after-char "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 4))))

  (testing "searching in groups of 14"
    (is (= 19 (first-marker-after-char "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14)))
    (is (= 23 (first-marker-after-char "bvwbjplbgvbhsrlpgdmjqwftvncz" 14)))
    (is (= 23 (first-marker-after-char "nppdvjthqldpwncqszvftbrmjlhg" 14)))
    (is (= 29 (first-marker-after-char "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 14)))
    (is (= 26 (first-marker-after-char "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 14)))))


