(ns aoc2022.09
  (:require [criterium.core :as c]
            [clojure.string :as str]))

(def sample-input (slurp "resources/09-sample-input.txt"))
(def real-input (slurp "resources/09-input.txt"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def input-str sample-input) ;; for REPL evaluations )

(defn parse-input [input-str]
  (->> input-str
       (str/split-lines)
       (mapv #(str/split % #" "))
       (mapcat (fn [[dir cnt-s]]
                 (let [cnt (parse-long cnt-s)]
                   (repeat cnt (keyword dir)))))
       vec))

(def move-vectors
  {:L [-1 0]
   :R [1 0]
   :U [0 1]
   :D [0 -1]})

(def hor-ver-tail-pull-vecs
  [[-1 0]
   [1 0]
   [0 -1]
   [0 1]])

(def diagonal-pull-vecs
  [[-1 1]
   [1 1]
   [1 -1]
   [-1 -1]])

(defn distance [[x1 y1] [x2 y2]]
  [(abs (- x1 x2)) (abs (- y1 y2))])

(defn max-distance [p1 p2]
  (apply max (distance p1 p2)))

(defn add-vec [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn maybe-pull-rope [[hx hy :as head] [tx ty :as tail]]
  (let [tail-pull-vecs (if (or (= hx tx) (= hy ty))
                         hor-ver-tail-pull-vecs
                         diagonal-pull-vecs)]
    (cond
      (<= (max-distance head tail) 1) tail
      :else (->> tail-pull-vecs
                 (map #(add-vec tail %))
                 (filter #(<= (max-distance % head) 1))
                 first))))

(defn next-state [{:keys [head tail tail-visited-coords]} move-dir] 
  (let [move-vec (get move-vectors move-dir)
        new-head (add-vec head move-vec)
        new-tail (maybe-pull-rope new-head tail)]
    {:head new-head
     :tail new-tail
     :tail-visited-coords (conj tail-visited-coords new-tail)}))

(defn solve-1 [input-str]
  (let [input-steps (parse-input input-str)]
    (->> (reduce
          next-state
          {:head [0 0]
           :tail [0 0]
           :tail-visited-coords []}
          input-steps)
         :tail-visited-coords
         set
         count)))

(comment
  (c/quick-bench (solve-1 real-input)))
  ;;   Evaluation count : 54 in 6 samples of 9 calls.
  ;;              Execution time mean : 13.474417 ms
  ;;     Execution time std-deviation : 2.197117 ms
  ;;    Execution time lower quantile : 11.897454 ms ( 2.5%)
  ;;    Execution time upper quantile : 16.174276 ms (97.5%)
  ;;                    Overhead used : 5.326449 ns
  

(defn next-state-generic [{:keys [coords tail-visited-coords]} move-dir]
  (let [move-vec (get move-vectors move-dir)
        transient-new-coords (transient coords)]
    (assoc! transient-new-coords 0 (add-vec (get transient-new-coords 0) move-vec))
    (doall (for [idx (drop 1 (range (count coords)))]
             (let [prev-block (nth transient-new-coords (dec idx))
                   this-block (nth transient-new-coords idx)]
               (assoc! transient-new-coords idx (maybe-pull-rope prev-block this-block)))))
    (let [new-coords (persistent! transient-new-coords)]
      {:coords new-coords
       :tail-visited-coords (conj tail-visited-coords (last new-coords))})))


(defn solve-generic [input-str chain-links]
  (let [input-steps (parse-input input-str)]
    (->> (reduce
          next-state-generic
          {:coords (vec (repeat chain-links [0 0]))
           :tail-visited-coords []}
          input-steps)
         :tail-visited-coords
         set
         count)))

(comment
  (c/quick-bench (solve-generic (slurp "resources/09-sample-input.txt") 2))
  ;; => 13
  ;; Evaluation count : 13566 in 6 samples of 2261 calls.
  ;;              Execution time mean : 51.472371 µs
  ;;     Execution time std-deviation : 8.336882 µs
  ;;    Execution time lower quantile : 44.937917 µs ( 2.5%)
  ;;    Execution time upper quantile : 61.845961 µs (97.5%)
  ;;                    Overhead used : 5.326449 ns
  
  (solve-generic (slurp "resources/09-sample-input-part2.txt") 10)
  ;; => 36
  
  (c/quick-bench (solve-generic real-input 10)))
  ;; => 2541
  ;; Evaluation count : 12 in 6 samples of 2 calls.
  ;;              Execution time mean : 64.991084 ms
  ;;     Execution time std-deviation : 10.922252 ms
  ;;    Execution time lower quantile : 57.474596 ms ( 2.5%)
  ;;    Execution time upper quantile : 80.485511 ms (97.5%)
  ;;                    Overhead used : 5.326449 ns
