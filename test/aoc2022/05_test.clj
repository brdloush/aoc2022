(ns aoc2022.05-test
  (:require [clojure.test :refer [deftest is are testing]]
            [aoc2022.05 :refer [parse-crates-line
                                split-crates-and-steps
                                parse-command-str
                                split-input-to-state-and-commands
                                move-piles
                                top-crates
                                solve-part-1
                                solve-part-2]]))

(def sample-input (slurp "resources/05-sample-input.txt"))

(deftest parse-crates-line-test
  (testing "every combination from sample input works"
    (are [expected input] (= expected (parse-crates-line input))
      [nil "D" nil] "    [D]    "
      ["N" "C" nil] "[N] [C]    "
      ["Z" "M" "P"] "[Z] [M] [P]")))

(deftest split-crates-and-steps-test
  (let [[setup-str steps-str] (split-crates-and-steps sample-input)]
    (is (= ["    [D]    "
            "[N] [C]    "
            "[Z] [M] [P]"
            " 1   2   3 "]
           setup-str))
    (is (= ["move 1 from 2 to 1"
            "move 3 from 1 to 3"
            "move 2 from 2 to 1"
            "move 1 from 1 to 2"]
           steps-str))))

(deftest parse-step-str-test
  (is (= {:qty 1
          :from 2
          :to 3}
         (parse-command-str "move 1 from 2 to 3"))))

(deftest split-input-to-state-and-commands-test
  (is (= {:commands [{:from 2, :qty 1, :to 1}
                     {:from 1, :qty 3, :to 3}
                     {:from 2, :qty 2, :to 1}
                     {:from 1, :qty 1, :to 2}],
          :piles [["Z" "N"] ["M" "C" "D"] ["P"]]}
         (split-input-to-state-and-commands sample-input))))

(deftest move-piles-test
  (let [initial-piles [["Z" "N"] ["M" "C" "D"] ["P"]]
        expected-after-1 [["Z" "N" "D"] ["M" "C"] ["P"]]
        expected-after-2 [[] ["M" "C"] ["P" "D" "N" "Z"]]
        expected-after-3 [["C" "M"] [] ["P" "D" "N" "Z"]]
        expected-after-4 [["C"] ["M"] ["P" "D" "N" "Z"]]]
    (testing "after 1st move"
      (is (= expected-after-1
             (move-piles initial-piles {:qty 1, :from 2, :to 1} false))))
    (testing "after 2nd move"
      (is (= expected-after-2
             (move-piles expected-after-1 {:qty 3, :from 1, :to 3} false))))
    (testing "after 3rd move"
      (is (= expected-after-3
             (move-piles expected-after-2 {:qty 2, :from 2, :to 1} false))))
    (testing "after 4th move"
      (is (= expected-after-4
             (move-piles expected-after-3 {:qty 1, :from 1, :to 2} false))))))

(deftest top-crates-test
  (is (= "CMZ"
         (top-crates [["C"] ["M"] ["P" "D" "N" "Z"]]))))

(deftest solve-part-1-test
  (is (= "CMZ"
         (solve-part-1 sample-input))))

(deftest solve-part-2-test
  (is (= "MCD"
         (solve-part-2 sample-input))))
