(ns aoc2022.06
  (:require [criterium.core :as c]))

(def sample-input (slurp "resources/06-sample-input.txt"))
(def real-input (slurp "resources/06-input.txt"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(comment
  (def input-str sample-input)) ;; for REPL evaluations 

(defn first-marker-after-char [input-str group-size]
  (let [subvectors (->> input-str
                        (partition-all group-size 1)
                        (map vec)
                        vec)]
    (->> (map (fn [idx subvector] [idx subvector]) (range) subvectors) 
         (filter (fn [[_ subvector]]
                   (= (count subvector)
                      (count (set subvector)))))
         ffirst 
         (+ group-size))))


(defn solve-part-1 [input-str]
  (first-marker-after-char input-str 4))

(defn solve-part-2 [input-str]
  (first-marker-after-char input-str 14))

(comment 
  (c/quick-bench (solve-part-1 real-input))
  ;; Evaluation count : 282 in 6 samples of 47 calls.
  ;;            Execution time mean : 2.441994 ms
  ;;   Execution time std-deviation : 356.298762 µs
  ;;  Execution time lower quantile : 2.129609 ms ( 2.5%)
  ;;  Execution time upper quantile : 2.943789 ms (97.5%)
  ;;                  Overhead used : 5.262738 ns
  
  (c/quick-bench (solve-part-2 real-input))
  ;; Evaluation count : 90 in 6 samples of 15 calls.
  ;;              Execution time mean : 7.734979 ms
  ;;     Execution time std-deviation : 1.084589 ms
  ;;    Execution time lower quantile : 6.794728 ms ( 2.5%)
  ;;    Execution time upper quantile : 8.933555 ms (97.5%)
  ;;                    Overhead used : 5.262738 ns 
  )