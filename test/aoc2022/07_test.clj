(ns aoc2022.07-test
  (:require [clojure.test :refer [deftest is]]
            [aoc2022.07 :refer [parse-input
                                change-dir
                                gather-files-by-dirs
                                solve-1
                                solve-2]]))

(def sample-input (slurp "resources/07-sample-input.txt"))

(deftest parse-input-test
  (is (= [{:command :cd, :dir "/"}
          {:command :ls,
           :output [{:name "a", :type :dir}
                    {:name "b.txt", :size 14848514, :type :file}
                    {:name "c.dat", :size 8504156, :type :file}
                    {:name "d", :type :dir}]}
          {:command :cd, :dir "a"}
          {:command :ls,
           :output [{:name "e", :type :dir}
                    {:name "f", :size 29116, :type :file}
                    {:name "g", :size 2557, :type :file}
                    {:name "h.lst", :size 62596, :type :file}]}
          {:command :cd, :dir "e"}
          {:command :ls, :output [{:name "i", :size 584, :type :file}]}
          {:command :cd, :dir ".."}
          {:command :cd, :dir ".."}
          {:command :cd, :dir "d"}
          {:command :ls,
           :output [{:name "j", :size 4060174, :type :file}
                    {:name "d.log", :size 8033020, :type :file}
                    {:name "d.ext", :size 5626152, :type :file}
                    {:name "k", :size 7214296, :type :file}]}]
         (parse-input sample-input))))

(deftest change-dir-test
  (is (= "/" (change-dir nil "/")))
  (is (= "/a/" (change-dir "/" "a")))
  (is (= "/a/b/" (change-dir "/a/" "b")))
  (is (= "/a/" (change-dir "/a/b/" ".."))))

(deftest gather-files-by-dirs-test
  (is (= {"/" [{:name "b.txt", :size 14848514, :type :file} 
               {:name "c.dat", :size 8504156, :type :file}],
          "/a/" [{:name "f", :size 29116, :type :file}
                 {:name "g", :size 2557, :type :file}
                 {:name "h.lst", :size 62596, :type :file}],
          "/a/e/" [{:name "i", :size 584, :type :file}],
          "/d/" [{:name "j", :size 4060174, :type :file}
                 {:name "d.log", :size 8033020, :type :file}
                 {:name "d.ext", :size 5626152, :type :file}
                 {:name "k", :size 7214296, :type :file}]}
         (gather-files-by-dirs [{:command :cd, :dir "/"}
                                {:command :ls,
                                 :output [{:name "a", :type :dir}
                                          {:name "b.txt", :size 14848514, :type :file}
                                          {:name "c.dat", :size 8504156, :type :file}
                                          {:name "d", :type :dir}]}
                                {:command :cd, :dir "a"}
                                {:command :ls,
                                 :output [{:name "e", :type :dir}
                                          {:name "f", :size 29116, :type :file}
                                          {:name "g", :size 2557, :type :file}
                                          {:name "h.lst", :size 62596, :type :file}]}
                                {:command :cd, :dir "e"}
                                {:command :ls, :output [{:name "i", :size 584, :type :file}]}
                                {:command :cd, :dir ".."}
                                {:command :cd, :dir ".."}
                                {:command :cd, :dir "d"}
                                {:command :ls,
                                 :output [{:name "j", :size 4060174, :type :file}
                                          {:name "d.log", :size 8033020, :type :file}
                                          {:name "d.ext", :size 5626152, :type :file}
                                          {:name "k", :size 7214296, :type :file}]}]))))

(deftest solve-1-test
  (is (= 95437
         (solve-1 sample-input))))

(deftest solve-2-test
  (is (= 24933642
         (solve-2 sample-input))))
