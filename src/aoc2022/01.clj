(ns aoc2022.01
  (:require [clojure.string :as str]
            [criterium.core :as c]))

(def sample-input (slurp "resources/01-sample-input.txt"))
(def real-input (slurp "resources/01-input.txt"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(comment
  (def input-str sample-input)) ;; for REPL evaluations 
  

(defn sorted-sums [input-str]
  (->> (str/split input-str #"\n\n")
       (mapv #(->> (str/split-lines %)
                   (mapv parse-long)))
       (map #(reduce + %))
       (sort >)))

(defn solution-part-1 [input]
  (first (sorted-sums input)))

(defn solution-part-2 [input]
  (reduce + (take 3 (sorted-sums input))))

(comment
  (c/quick-bench (solution-part-1 real-input))
  ;; => 71471
  
  ;; Evaluation count : 2568 in 6 samples of 428 calls.
  ;;              Execution time mean : 275.528032 µs
  ;;     Execution time std-deviation : 39.738816 µs
  ;;    Execution time lower quantile : 244.187516 µs ( 2.5%)
  ;;    Execution time upper quantile : 322.412439 µs (97.5%)
  ;;                    Overhead used : 5.326808 ns  
  
  (c/quick-bench (solution-part-2 real-input))
  ;; => 211189
  
  ;; Evaluation count : 2526 in 6 samples of 421 calls.
  ;;            Execution time mean : 282.365480 µs
  ;;   Execution time std-deviation : 38.824595 µs
  ;;  Execution time lower quantile : 248.097701 µs ( 2.5%)
  ;;  Execution time upper quantile : 332.789675 µs (97.5%)
  ;;                  Overhead used : 5.326808 ns
  )