(ns aoc2022.09-test
  (:require [clojure.test :refer [deftest testing is]]
            [aoc2022.09 :refer [parse-input
                                maybe-pull-rope
                                next-state
                                solve-1
                                solve-generic]]))

(def sample-input (slurp "resources/09-sample-input.txt"))
(def sample-input-part-2 (slurp "resources/09-sample-input-part2.txt"))

(deftest parse-input-test
  (is (= [:R :R :R :R :U :U :U :U :L :L :L :D :R :R :R :R :D :L :L :L :L :L :R :R]
         (parse-input sample-input))))

(deftest maybe-pull-rope-test
  (testing "combinations not requiring pull"
    (is (= [0 0] (maybe-pull-rope [0 0] [0 0])))
    (is (= [-1 0] (maybe-pull-rope [0 0] [-1 0])))
    (is (= [1 0] (maybe-pull-rope [0 0] [1 0])))
    (is (= [0 -1] (maybe-pull-rope [0 0] [0 -1])))
    (is (= [0 1] (maybe-pull-rope [0 0] [0 1])))
    (is (= [-1 -1] (maybe-pull-rope [0 0] [-1 -1])))
    (is (= [1 1] (maybe-pull-rope [0 0] [1 1]))))
  (testing "combinations resulting in horizontal/vertical pull"
    (is (= [0 -1] (maybe-pull-rope [0 0] [0 -2])))
    (is (= [0 1] (maybe-pull-rope [0 0] [0 2])))
    (is (= [-1 0] (maybe-pull-rope [0 0] [-2 0])))
    (is (= [1 0] (maybe-pull-rope [0 0] [2 0]))))
  (testing "combinations resulting in diagonal pull"
    (is (= [+1 +1] (maybe-pull-rope [+1 +2] [0 0])))
    (is (= [+1 +1] (maybe-pull-rope [+2 +1] [0 0])))
    (is (= [+1 -1] (maybe-pull-rope [+1 -2] [0 0])))
    (is (= [+1 -1] (maybe-pull-rope [+2 -1] [0 0])))
    (is (= [-1 -1] (maybe-pull-rope [-1 -2] [0 0])))
    (is (= [-1 -1] (maybe-pull-rope [-2 -1] [0 0])))
    (is (= [-1 +1] (maybe-pull-rope [-2 +1] [0 0])))
    (is (= [-1 +1] (maybe-pull-rope [-1 +2] [0 0])))))

(deftest next-state-test
  (testing "1 step move after being at the same position doesn't need tail to move"
    (is (= {:head [0 1] :tail [0 0] :tail-visited-coords [[0 0]]}
           (next-state {:head [0 0], :tail [0 0]} :U)))
    (is (= {:head [1 0] :tail [0 0] :tail-visited-coords [[0 0]]}
           (next-state {:head [0 0], :tail [0 0]} :R)))
    (is (= {:head [0 -1] :tail [0 0] :tail-visited-coords [[0 0]]}
           (next-state {:head [0 0], :tail [0 0]} :D)))
    (is (= {:head [-1 0] :tail [0 0] :tail-visited-coords [[0 0]]}
           (next-state {:head [0 0], :tail [0 0]} :L))))
  (testing "few samples from aoc site"
    (is (= {:head [4 0], :tail [3 0], :tail-visited-coords [[0 0] [1 0] [2 0] [3 0]]}
           (-> {:head [0 0]
                :tail [0 0]
                :tail-visited-coords []}
               (next-state :R)
               (next-state :R)
               (next-state :R)
               (next-state :R))))))

(deftest solve-1-test
  (is (= 13
         (solve-1 sample-input))))

(deftest solve-generic-test
  (is (= 13
         (solve-generic sample-input 2)))
  (is (= 36
         (solve-generic sample-input-part-2 10))))