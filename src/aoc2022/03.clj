(ns aoc2022.03
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [criterium.core :as c]))

(def sample-input (slurp "resources/03-sample-input.txt"))
(def real-input (slurp "resources/03-input.txt"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(comment
  (def input-str sample-input)) ;; for REPL evaluations 

(defn split-to-half-sets [s]
  (let [half-idx (/ (count s) 2)
        p1 (set (map str (take half-idx s)))
        p2 (set (map str (drop half-idx s)))]
    [p1 p2]))

(defn shared-element [s]
  (let [[p1 p2] (split-to-half-sets s)
        shared (set/intersection p1 p2)]
    (first shared)))

(def lowercase-alpha-chars-vec (->> (range (int \a) (inc (int \z))) (mapv (comp str char))))

(defn priority [c]
  (let [lowered-c (str/lower-case c)
        is-uppercase (not= lowered-c c)
        base-priority (if is-uppercase 27 1)
        idx-of-alphabet (.indexOf lowercase-alpha-chars-vec lowered-c)] 
    (+ base-priority idx-of-alphabet)))


(defn solution-1 [input-str]
  (->> input-str
       str/split-lines
       (map shared-element)
       (map priority)
       (apply +)))

(defn solution-2 [input-str]
  (->> input-str
       str/split-lines
       (map #(set (map str %)))
       (partition 3)
       (map (fn shared-letter-in-groups [group-lines]
              (first (apply set/intersection group-lines))))
       (map priority)
       (apply +)))

(comment
  (c/quick-bench (solution-1 real-input))

  ;; Evaluation count : 192 in 6 samples of 32 calls.
  ;;            Execution time mean : 3.572977 ms
  ;;   Execution time std-deviation : 510.412815 µs
  ;;  Execution time lower quantile : 3.065389 ms ( 2.5%)
  ;;  Execution time upper quantile : 4.308323 ms (97.5%)
  ;;                  Overhead used : 5.314211 ns
  ;; => 7845
  
  (c/quick-bench (solution-2 real-input))
  ;; Evaluation count : 234 in 6 samples of 39 calls.
  ;;            Execution time mean : 2.546514 ms
  ;;   Execution time std-deviation : 61.664691 µs
  ;;  Execution time lower quantile : 2.480669 ms ( 2.5%)
  ;;  Execution time upper quantile : 2.630136 ms (97.5%)
  ;;                  Overhead used : 5.314211 ns
  )
