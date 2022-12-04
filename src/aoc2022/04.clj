(ns aoc2022.04
  (:require [clojure.string :as str]
            [criterium.core :as c]))

(def sample-input (slurp "resources/04-sample-input.txt"))
(def real-input (slurp "resources/04-input.txt"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(comment
  (def input-str sample-input)) ;; for REPL evaluations 

(defn sort-intervals [interval1 interval2]
  (let [[from1 to1] interval1
        [from2 to2] interval2]
    (cond
      (< from1 from2) [interval1 interval2]
      (> from1 from2) [interval2 interval1]
      :else (let [r1-count (- to1 from1)
                  r2-count (- to2 from2)]
              (cond
                (< r1-count r2-count) [interval1 interval2]
                (> r1-count r2-count) [interval2 interval1]
                :else [interval1 interval2])))))

(defn shared-interval [interval1 interval2]
  (if (= interval1 interval2)
    interval1
    (let [[[_from1 to1] [from2 to2]] (sort-intervals interval1 interval2)]
      (cond
        (< to1 from2) nil
        (= to1 from2) [to1 to1]
        :else [from2 (min to1 to2)]))))

(defn parse-line [l]
  (->> (str/split l #",")
       (mapv (fn parse-interval [i]
               (->> (str/split i #"-")
                    (mapv parse-long))))))

(defn fully-contained [[from1 to1] [from2 to2]]
  (or
   (<= from1 from2 to2 to1)
   (<= from2 from1 to1 to2)))

(defn solution-1 [input-str]
  (->> input-str
       (str/split-lines)
       (mapv parse-line)
       (map #(apply fully-contained %))
       (filter true?)
       count))

(comment
  (c/quick-bench (solution-1 real-input))
  ;;   Evaluation count : 786 in 6 samples of 131 calls.
  ;;              Execution time mean : 886.029978 µs
  ;;     Execution time std-deviation : 146.301492 µs
  ;;    Execution time lower quantile : 797.437687 µs ( 2.5%)
  ;;    Execution time upper quantile : 1.137678 ms (97.5%)
  ;;                    Overhead used : 5.314211 ns
  ;; Found 1 outliers in 6 samples (16.6667 %)
  ;; 	low-severe	 1 (16.6667 %)
  ;;  Variance from outliers : 47.6121 % Variance is moderately inflated by outliers
  )

(defn solution-2 [input-str]
  (->> input-str
       (str/split-lines)
       (mapv parse-line)
       (map #(apply shared-interval %))
       (remove nil?)
       count))

(comment
  (c/quick-bench (solution-2 real-input))
  ;; Evaluation count : 702 in 6 samples of 117 calls.
  ;;            Execution time mean : 960.959769 µs
  ;;   Execution time std-deviation : 188.083049 µs
  ;;  Execution time lower quantile : 836.945513 µs ( 2.5%)
  ;;  Execution time upper quantile : 1.223964 ms (97.5%)
  ;;                  Overhead used : 5.314211 ns
  )