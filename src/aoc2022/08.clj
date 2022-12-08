(ns aoc2022.08
  (:require [criterium.core :as c]
            [clojure.string :as str]))

(def sample-input (slurp "resources/08-sample-input.txt"))
(def real-input (slurp "resources/08-input.txt"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(comment
  (def input-str sample-input)) ;; for REPL evaluations )

(defn parse-input-to-indexed-nums [input-str]
  (let [vec-items (->> input-str
                       (str/split-lines)
                       (mapv (fn [line]
                               (mapv (comp parse-long str) line))))]
    (into (sorted-map)
          (for [y (range (count vec-items))
                x (range (count (first vec-items)))]
            [[x y] (-> vec-items (nth y) (nth x))]))))

(defn solve-1 [input-str]
  (let [indexed-trees (parse-input-to-indexed-nums input-str)
        width (->> indexed-trees (map first) (map first) (apply max) inc)
        height (->> indexed-trees (map first) (map second) (apply max) inc)]
    (->> (for [y (range height)
               x (range width)]
           (let [this-size (get indexed-trees [x y])]
             [(->> (for [y (range 0 y)] [x y])
                   (mapv #(get indexed-trees %))
                   (every? #(< % this-size)))
              (->> (for [y (range (inc y) height)] [x y])
                   (mapv #(get indexed-trees %))
                   (every? #(< % this-size)))
              (->> (for [x (range 0 x)] [x y])
                   (mapv #(get indexed-trees %))
                   (every? #(< % this-size)))
              (->> (for [x (range (inc x) width)] [x y])
                   (mapv #(get indexed-trees %))
                   (every? #(< % this-size)))]))
         (map #(some true? %))
         (filter true?)
         count)))

(comment 
  (c/quick-bench (solve-1 real-input))
  ;; Evaluation count : 6 in 6 samples of 1 calls.
  ;;              Execution time mean : 383.898317 ms
  ;;     Execution time std-deviation : 2.250095 ms
  ;;    Execution time lower quantile : 380.597731 ms ( 2.5%)
  ;;    Execution time upper quantile : 386.159779 ms (97.5%)
  ;;                    Overhead used : 5.235788 ns
  )

(defn trees-in-direction [indexed-trees [base-x base-y] [inc-x inc-y]]
  (let [base-size (get indexed-trees [base-x base-y])]
    (loop [x (+ base-x inc-x)
           y (+ base-y inc-y)
           trees 0]
      (let [evaluated-tree-size (get indexed-trees [x y])]
        ;; (println "evaluated-tree-size" evaluated-tree-size)
        (cond
          (= evaluated-tree-size nil) trees
          (>= evaluated-tree-size base-size) (inc trees)
          :else (recur (+ x inc-x) (+ y inc-y) (inc trees)))))))

(defn scenic-score-for [indexed-trees [x y]]
  (->> [(trees-in-direction indexed-trees [x y] [0 -1]) ; up
        (trees-in-direction indexed-trees [x y] [-1 0]) ; left
        (trees-in-direction indexed-trees [x y] [1 0])  ; right 
        (trees-in-direction indexed-trees [x y] [0 1])  ; down
        ]
       (apply *)))

(defn solve-2 [input-str]
  (let [indexed-trees (parse-input-to-indexed-nums input-str)
        width (->> indexed-trees (map first) (map first) (apply max) inc)
        height (->> indexed-trees (map first) (map second) (apply max) inc)]
    (->> (for [y (range height)
               x (range width)]
           (scenic-score-for indexed-trees [x y]))
         (apply max))
    ))

(comment
  (solve-2 sample-input)
  ;; => 8

  (c/quick-bench (solve-2 real-input))
  ;; Evaluation count : 18 in 6 samples of 3 calls.
  ;;              Execution time mean : 44.921042 ms
  ;;     Execution time std-deviation : 863.182908 µs
  ;;    Execution time lower quantile : 43.468339 ms ( 2.5%)
  ;;    Execution time upper quantile : 45.754969 ms (97.5%)
  ;;                    Overhead used : 5.235788 ns
  )
