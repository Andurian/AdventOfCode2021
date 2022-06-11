(ns advent-of-code-2021.day04
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string]
            [clojure.set])
  (:gen-class))

(defn- any-true [coll]
  (if (= (some #(= % true) coll) true) true false))

(defn- transpose [m]
  (apply mapv vector m))

(defn- read-row-vector [line]
  (let [tokens (clojure.string/split line #"\s+")]
    (->> tokens
         (filter seq)
         (map read-string)
         (vec))))

(defn- read-matrix [lines]
  (vec (map read-row-vector lines)))

(defn- read-bingo-numbers [string]
  (map read-string (clojure.string/split string #",")))

(defn- set-from-bingo-board [board]
  (let [transposed (transpose board)
        horizontal-sets (map set board)
        vertical-sets (map set transposed)]
    (concat horizontal-sets vertical-sets)))

(defn- read-bingo-boards [lines]
  (let [boards (map rest (partition 6 lines))
        board-matrices (map read-matrix boards)
        board-sets (map set-from-bingo-board board-matrices)]
    board-sets))

(defn- read-input [lines]
  (let [bingo-numbers (read-bingo-numbers (first lines))
        bingo-boards (read-bingo-boards (rest lines))]
    {:numbers bingo-numbers
     :boards bingo-boards}))

(defn- bingo? [number-set board]
  (any-true (map #(clojure.set/subset? % number-set) board)))

(defn- get-board-with-bingo [number-set boards]
  (loop [i 0]
    (cond
      (>= i (count boards)) nil
      (bingo? number-set (nth boards i)) (nth boards i)
      :else (recur (inc i)))))

(defn- winning-numbers [numbers-set winning-board last-called]
  (let [board-numbers (apply clojure.set/union winning-board)
        unmarked-numbers (clojure.set/difference board-numbers numbers-set)
        sum-unmarked (reduce + unmarked-numbers)]
    (* last-called sum-unmarked)))

(defn- index-of-bingo [numbers board]
  (loop [n 5]
    (cond
      (>= n (count numbers)) -1
      (bingo? (set (take n numbers)) board) n
      :else (recur (inc n)))))

(defn- task02 [lines]
  (let [input (read-input lines)
        numbers (input :numbers)
        boards (input :boards)
        bingo-after (map #(index-of-bingo numbers %) boards)
        max-value (apply max bingo-after)
        max-index (.indexOf bingo-after max-value)
        last-called (nth numbers (dec max-value))]
    (winning-numbers (set (take max-value numbers)) (nth boards max-index) last-called)))

(defn- task01 [lines]
  (let [input (read-input lines)
        numbers (input :numbers)
        boards (input :boards)]
    (loop [n 5]
      (if (>= n (count numbers))
        (println "NO BINGO FOUND");
        (let [numbers-set (set (take n numbers))
              board-with-bingo (get-board-with-bingo numbers-set boards)]
          (if (= nil board-with-bingo)
            (recur (inc n))
            (winning-numbers numbers-set board-with-bingo (nth numbers (- n 1)))))))))

(defn day04
  ([] (day04 "input_day_04.txt"))
  ([filename]
   (let [lines (read-lines filename)]
     (println "Solution Day 04-1:" (task01 lines))
     (println "Solution Day 04-2:" (task02 lines)))))

(defn -main [] (time (day04)))