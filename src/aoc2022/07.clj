(ns aoc2022.07
  (:require [criterium.core :as c]
            [clojure.string :as str]))

(def sample-input (slurp "resources/07-sample-input.txt"))
(def real-input (slurp "resources/07-input.txt"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(comment
  (def input-str sample-input)) ;; for REPL evaluations 

(defn resolve-ls-output [s]
  (let [[dir-match dir-name] (re-find #"^dir (.+)$" s)
        [file-match file-size file-name] (re-find #"^(\d+) (.+)$" s)]
    (cond
      dir-match {:type :dir
                 :name dir-name}
      file-match {:type :file
                  :name file-name
                  :size (parse-long file-size)})))
                
(defn resolve-command [cmd output]
  (let [[cd-match cd-dir] (re-find #"^cd (.+)$" cmd)
        [ls-match] (re-find #"^ls$" cmd)]
    (cond
      cd-match {:command :cd
                :dir cd-dir}
      ls-match {:command :ls
                :output (->> output 
                             (mapv resolve-ls-output))})))

(defn parse-input [input-str]
  (->> (str/split input-str #"[$] ")
       (remove str/blank?)
       (mapv str/split-lines)
       (mapv (fn [[cmd & rest]]
               (resolve-command cmd rest)))))

(defn change-dir [prev-dir cd-val]
  (cond
    (str/starts-with? cd-val "/") cd-val
    (= ".." cd-val) (let [dir-without-trailing-slash (subs prev-dir 0 (dec (count prev-dir)))]
                      (-> (subs dir-without-trailing-slash 0 (str/last-index-of dir-without-trailing-slash "/"))
                          (str "/")))
    :else (str prev-dir cd-val "/")))

(defn gather-files-by-dirs [parsed-input]
  (-> (reduce
       (fn [{:keys [current-dir] :as state} {:keys [command dir output]}] 
         (condp = command
           :cd (let [new-dir (change-dir current-dir dir)]
                 (assoc state :current-dir new-dir))
           :ls (let [files (->> output
                                (filter #(= (:type %) :file))
                                vec)]
                 (-> state
                     (update :gathered-files assoc current-dir files)))
           :else state))
       {:current-dir nil
        :gathered-files {}}
       parsed-input)
      :gathered-files))

(defn solve-1 [input-str]
   (let [files-by-dirs (->> (parse-input input-str)
                            (gather-files-by-dirs))
         direct-dir-files-size (->> files-by-dirs
                                    (map (fn [[dir files]]
                                           [dir (->> files
                                                     (map :size)
                                                     (apply +))])))
         cummulated-dir-size (->> direct-dir-files-size
                                  (map (fn [[dir _]]
                                         [dir
                                          (->> direct-dir-files-size
                                               (filter (fn [[other-dir _other-size]]
                                                         (str/includes? other-dir dir)))
                                               (map second)
                                               (apply +))])))]
     (->> cummulated-dir-size
          (map second)
          (filter #(> 100000 %))
          (apply +)))) 
 
(defn solve-2 [input-str]
  (let [files-by-dirs (->> (parse-input input-str)
                           (gather-files-by-dirs))
        direct-dir-files-size (->> files-by-dirs
                                   (map (fn [[dir files]]
                                          [dir (->> files
                                                    (map :size)
                                                    (apply +))])))
        cummulated-dir-sizes (->> direct-dir-files-size
                                 (map (fn [[dir _]]
                                        [dir
                                         (->> direct-dir-files-size
                                              (filter (fn [[other-dir _other-size]]
                                                        (str/includes? other-dir dir)))
                                              (map second)
                                              (apply +))]))
                                 (into {}))
        total-disk-size 70000000
        update-size 30000000
        root-dir-size (get cummulated-dir-sizes "/")
        disk-available (- total-disk-size root-dir-size)
        missing-size (- update-size disk-available)]
    
    (->> cummulated-dir-sizes 
         (sort-by second <)
         (filter (fn [[_ size]]
                   (>= size missing-size)))
         first
         second)))

(comment
  (solve-1 sample-input)
  ;; => 95437 
  
  (c/quick-bench (solve-1 real-input))
  ;;   Evaluation count : 180 in 6 samples of 30 calls.
  ;;            Execution time mean : 3.751806 ms
  ;;   Execution time std-deviation : 316.150871 µs
  ;;  Execution time lower quantile : 3.511483 ms ( 2.5%)
  ;;  Execution time upper quantile : 4.116271 ms (97.5%)
  ;;                  Overhead used : 5.235788 ns

  (solve-2 sample-input)
  ;; => 24933642
  
  (c/quick-bench (solve-2 real-input))
  ;; Evaluation count : 168 in 6 samples of 28 calls.
  ;;            Execution time mean : 3.919426 ms
  ;;   Execution time std-deviation : 324.855114 µs
  ;;  Execution time lower quantile : 3.664506 ms ( 2.5%)
  ;;  Execution time upper quantile : 4.386214 ms (97.5%)
  ;;                  Overhead used : 5.235788 ns

  )
