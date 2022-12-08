(ns aoc2022.08-test
  (:require [clojure.test :refer [deftest testing is are]]
            [aoc2022.08 :refer [parse-input-to-indexed-nums]]))

(def sample-input (slurp "resources/08-sample-input.txt"))

(deftest parse-input-to-indexed-nums-test
  (is (= {[0 0] 3,
          [0 1] 2,
          [0 2] 6,
          [0 3] 3,
          [0 4] 3,
          [1 0] 0,
          [1 1] 5,
          [1 2] 5,
          [1 3] 3,
          [1 4] 5,
          [2 0] 3,
          [2 1] 5,
          [2 2] 3,
          [2 3] 5,
          [2 4] 3,
          [3 0] 7,
          [3 1] 1,
          [3 2] 3,
          [3 3] 4,
          [3 4] 9,
          [4 0] 3,
          [4 1] 2,
          [4 2] 2,
          [4 3] 9,
          [4 4] 0}
       (parse-input-to-indexed-nums sample-input))))

