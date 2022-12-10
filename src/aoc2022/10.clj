(ns aoc2022.10
  (:require [criterium.core :as c]
            [clojure.string :as str]))

(def sample-input (slurp "resources/10-sample-input.txt"))
(def real-input (slurp "resources/10-input.txt"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def input-str sample-input) ;; for REPL evaluations 


(def cmd-cycles
  {:noop 1
   :addx 2})

(defn parse-command [s]
  (let [[cmd value] (str/split s #" ")
        cmd-kw (keyword cmd)]
    {:cmd cmd-kw
     :value (when value (parse-long value))
     :cycles (get cmd-cycles cmd-kw)}))

(defn parse-commands [input-str]
  (->> input-str
       str/split-lines
       (mapv parse-command)))

(defn apply-command [state
                     {:keys [cmd value cycles] :as command}]
  (let [inc-x (if (= cmd :addx) value
                  0)]
    (-> state
        (update :x + inc-x)
        (update :cycle-number + cycles)
        (update :visited-states conj (dissoc state :visited-states)))))

(defn lookup-closest-smaller-state [states-by-cycles requested-cycle]
  (if (contains? states-by-cycles requested-cycle)
    (get states-by-cycles requested-cycle)
    (lookup-closest-smaller-state states-by-cycles (dec requested-cycle))))

(defn solve-1 [input-str]
  (let [commands (parse-commands input-str)
        visited-states (->> (reduce
                             apply-command
                             {:x 1, :cycle-number 1, :visited-states []}
                             commands)
                            :visited-states)
        states-by-cycles (->> visited-states
                              (group-by :cycle-number)
                              (map (juxt first (comp first second)))
                              (into (sorted-map)))]
    (->> [20 60 100 140 180 220]
         (map (fn [requested-cycle]
                [requested-cycle (:x (lookup-closest-smaller-state states-by-cycles requested-cycle))]))
         (map #(apply * %))
         (apply +))))

(comment
  (solve-1 sample-input)
;; => 13140

  (c/quick-bench (solve-1 real-input)))
  ;; => 12980
;;   Evaluation count : 4092 in 6 samples of 682 calls.
;;              Execution time mean : 171.714915 µs
;;     Execution time std-deviation : 24.804690 µs
;;    Execution time lower quantile : 149.805941 µs ( 2.5%)
;;    Execution time upper quantile : 206.887407 µs (97.5%)
;;                    Overhead used : 5.326449 ns

(defn solve-2 [input-str]
  (let [commands (parse-commands input-str)
        visited-states (->> (reduce
                             apply-command
                             {:x 1, :cycle-number 1, :visited-states []}
                             commands)
                            :visited-states)
        states-by-cycles (->> visited-states
                              (group-by :cycle-number)
                              (map (juxt first (comp first second)))
                              (into (sorted-map)))]
    (->> (range 1 241)
         (map (fn [idx]
                (let [{:keys [x]} (lookup-closest-smaller-state states-by-cycles idx)
                      sprite-idxs #{(dec x) x (inc x)}
                      display-idx (mod (dec idx) 40)]
                  [display-idx sprite-idxs (if (contains? sprite-idxs display-idx) "#" ".")])))
         (map #(nth % 2))
         (partition-all 40)
         (map str/join))))

(comment
  (solve-2 input-str)
  ;; => ("##..##..##..##..##..##..##..##..##..##.."
  ;;     "###...###...###...###...###...###...###."
  ;;     "####....####....####....####....####...."
  ;;     "#####.....#####.....#####.....#####....."
  ;;     "######......######......######......####"
  ;;     "#######.......#######.......#######.....")


  (c/quick-bench (solve-2 real-input))
  ;; => ("###..###....##.#....####.#..#.#....###.."
  ;;     "#..#.#..#....#.#....#....#..#.#....#..#."
  ;;     "###..#..#....#.#....###..#..#.#....#..#."
  ;;     "#..#.###.....#.#....#....#..#.#....###.."
  ;;     "#..#.#.#..#..#.#....#....#..#.#....#...."
  ;;     "###..#..#..##..####.#.....##..####.#....")
  
;;   Evaluation count : 4134 in 6 samples of 689 calls.
;;              Execution time mean : 171.272339 µs
;;     Execution time std-deviation : 28.666743 µs
;;    Execution time lower quantile : 144.943463 µs ( 2.5%)
;;    Execution time upper quantile : 210.897049 µs (97.5%)
;;                    Overhead used : 5.326449 ns
  )
