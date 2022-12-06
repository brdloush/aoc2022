(ns aoc2022.05
  (:require [clojure.string :as str]
            [criterium.core :as c]))

(def sample-input (slurp "resources/05-sample-input.txt"))
(def real-input (slurp "resources/05-input.txt"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(comment
  (def input-str sample-input)) ;; for REPL evaluations 

(defn parse-crates-line [s]
  (->> s
       (partition-all 4)
       (map str/join)
       (mapv #(re-find #"[A-Z]" %))))

(defn split-crates-and-steps [input-str]
  (let [[setup-str _ steps-str] (->> input-str
                                     (str/split-lines)
                                     (partition-by str/blank?))]
    [setup-str steps-str]))

(defn parse-command-str [step-str]
  (let [[qty from to] (->> (re-find #"move (\d+) from (\d+) to (\d+)" step-str)
                           (drop 1)
                           (map parse-long))]
    {:qty qty
     :from from
     :to to}))

(defn transpose [m]
  (apply mapv vector m))

(defn split-input-to-state-and-commands [input-str]
  (let [[piles-str commands-str] (->> input-str
                                      split-crates-and-steps)]
    {:piles (->> piles-str
                 butlast ; last line of piles part contains just the incremental indices
                 (map parse-crates-line)
                 vec
                 transpose
                 (mapv #(remove nil? %))
                 (mapv reverse))
     :commands (mapv parse-command-str commands-str)}))

(defn move-piles [piles {:keys [qty from to] :as _tranfer-commend} move-crates-at-once]
  (let [src-pile-idx (dec from)
        dest-pile-idx (dec to)
        source-pile (nth piles (dec from))
        move-crates-fn (if move-crates-at-once identity reverse)
        [new-source-pile transferred-items] (split-at (- (count source-pile) qty) source-pile)]
    (-> piles
        (assoc src-pile-idx (vec new-source-pile))
        (update dest-pile-idx #(vec (concat % (move-crates-fn transferred-items)))))))

(defn top-crates [piles]
  (->> piles
       (map last)
       str/join))

(defn solve-part-1 [input-str]
  (let [{:keys [piles commands]} (split-input-to-state-and-commands input-str)
        final-piles (reduce (fn [prev-state command]
                              (move-piles prev-state command false))
                            piles
                            commands)]
    (top-crates final-piles)))

(defn solve-part-2 [input-str]
  (let [{:keys [piles commands]} (split-input-to-state-and-commands input-str)
        final-piles (reduce (fn [prev-state command]
                              (move-piles prev-state command true))
                            piles
                            commands)]
    (top-crates final-piles)))

(comment
  (solve-part-1 sample-input)

  (c/quick-bench (solve-part-1 real-input))
  ;; Evaluation count : 588 in 6 samples of 98 calls.
  ;;            Execution time mean : 1.168864 ms
  ;;   Execution time std-deviation : 199.017483 µs
  ;;  Execution time lower quantile : 1.012605 ms ( 2.5%)
  ;;  Execution time upper quantile : 1.403912 ms (97.5%)
  ;;                  Overhead used : 5.262738 ns 

  (c/quick-bench (solve-part-2 real-input)))
  ;; Evaluation count : 618 in 6 samples of 103 calls.
  ;;            Execution time mean : 1.124605 ms
  ;;   Execution time std-deviation : 181.503043 µs
  ;;  Execution time lower quantile : 991.445874 µs ( 2.5%)
  ;;  Execution time upper quantile : 1.348000 ms (97.5%)
  ;;                  Overhead used : 5.262738 ns
