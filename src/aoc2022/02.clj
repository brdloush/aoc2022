(ns aoc2022.02
  (:require [clojure.string :as str]
            [criterium.core :as c]))

(def sample-input (slurp "resources/02-sample-input.txt"))
(def real-input (slurp "resources/02-input.txt"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(comment
  (def input-str sample-input)) ;; for REPL evaluations 

(defn decode-opponent-shape [code]
  (condp = code
    "A" :rock
    "B" :paper
    "C" :scissors))

(defn decode-column2-part1 [code]
  (condp = code
    "X" :rock
    "Y" :paper
    "Z" :scissors))

(defn winning-shape-against [opponent-shape]
  (condp = opponent-shape
    :rock :paper
    :paper :scissors
    :scissors :rock))

(defn losing-shape-against [opponent-shape]
  (condp = opponent-shape
    :paper :rock
    :scissors :paper
    :rock :scissors))

(defn what-response [opponent-shape desired-result]
  (condp = desired-result
    :draw opponent-shape
    :win (winning-shape-against opponent-shape)
    :lose (losing-shape-against opponent-shape)))

(defn shape-score [shape]
  (condp = shape
    :rock 1
    :paper 2
    :scissors 3))

(defn result-score [result]
  (condp = result
    :lose 0
    :draw 3
    :win 6))

(defn round-result [opponent-shape my-shape]
  (let [round [opponent-shape my-shape]]
    (cond
      (= my-shape opponent-shape) :draw
      (= [:scissors :rock] round) :win
      (= [:rock :paper] round) :win
      (= [:paper :scissors] round) :win
      :else :lose)))

(defn round-score [opponent-shape my-shape]
  (result-score (round-result opponent-shape my-shape)))

(defn solution-1 [input-str]
  (->> input-str
       (str/split-lines)
       (mapv (fn ->coded-rows [line]
               (->> (str/split line #" "))))
       (mapv (fn ->total-scores [[col1 col2]]
               (let [opponent-shape (decode-opponent-shape col1)
                     my-shape (decode-column2-part1 col2)]
                 (+ (round-score opponent-shape my-shape) (shape-score my-shape)))))
       (apply +)))

(comment
  (c/quick-bench (solution-1 real-input)))
;; => 11666

;; Evaluation count : 522 in 6 samples of 87 calls.
;;              Execution time mean : 1.184541 ms
;;     Execution time std-deviation : 26.907801 µs
;;    Execution time lower quantile : 1.145183 ms ( 2.5%)
;;    Execution time upper quantile : 1.212717 ms (97.5%)
;;                    Overhead used : 5.314211 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 30.9385 % Variance is moderately inflated by outliers

(defn decode-column2-part2 [c]
  (condp = c
    "X" :lose
    "Y" :draw
    "Z" :win))

(defn solution-2 [input-str]
  (->> input-str
       (str/split-lines)
       (mapv (fn ->coded-rows [line]
               (->> (str/split line #" "))))
       (mapv (fn [[opponent-code result-code]]
               [(decode-opponent-shape opponent-code) (decode-column2-part2 result-code)]))
       (mapv (fn [[opponent-shape result]]
               (let [my-shape (what-response opponent-shape result)]
                 (+ (result-score result) (shape-score my-shape)))))
       (apply +)))

(comment
  (c/quick-bench (solution-2 real-input)))

;; => 12767               

;; Evaluation count : 534 in 6 samples of 89 calls.
;;              Execution time mean : 1.154038 ms
;;     Execution time std-deviation : 15.311769 µs
;;    Execution time lower quantile : 1.135028 ms ( 2.5%)
;;    Execution time upper quantile : 1.170022 ms (97.5%)
;;                    Overhead used : 5.314211 ns
