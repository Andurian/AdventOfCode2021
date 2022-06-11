(ns advent-of-code-2021.day08
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string]
            [clojure.set])
  (:gen-class))

(defn- filter-by-size [numbers size]
  (filter #(= size (count %)) numbers))

(defn find-1 [numbers]
  (first (filter-by-size numbers 2)))

(defn find-7 [numbers]
  (first (filter-by-size numbers 3)))

(defn find-4 [numbers]
  (first  (filter-by-size numbers 4)))

(defn find-8 [numbers]
  (first  (filter-by-size numbers 7)))

(defn find-9 [numbers num-4]
  (let [with-6-segments (filter-by-size numbers 6)]
    (first (filter #(= num-4 (clojure.set/intersection % num-4)) with-6-segments))))

(defn find-2 [numbers num-9]
  (let [with-5-segments (filter-by-size numbers 5)]
    (first (filter #(= 7 (count (clojure.set/union % num-9))) with-5-segments))))

(defn find-5 [numbers num-2]
  (let [with-5-segments (filter-by-size numbers 5)]
    (first (filter #(= 7 (count (clojure.set/union % num-2))) with-5-segments))))

(defn find-3 [numbers num-2 num-5]
  (let [with-5-segments (filter-by-size numbers 5)]
    (first (filter #(and (not (= num-2 %)) (not (= num-5 %))) with-5-segments))))

(defn find-6 [numbers num-5 num-9]
  (let [with-6-segments (filter-by-size numbers 6)]
    (first (filter #(and (not (= num-9 %)) (= 5 (count (clojure.set/intersection % num-5)))) with-6-segments))))

(defn find-0 [numbers num-6 num-9]
  (let [with-6-segments (filter-by-size numbers 6)]
    (first (filter #(and (not (= num-6 %)) (not (= num-9 %))) with-6-segments))))

(defn fill-map [all-numbers]
  (let [num-1 (find-1 all-numbers)
        num-4 (find-4 all-numbers)
        num-7 (find-7 all-numbers)
        num-8 (find-8 all-numbers)
        num-9 (find-9 all-numbers num-4)
        num-2 (find-2 all-numbers num-9)
        num-5 (find-5 all-numbers num-2)
        num-3 (find-3 all-numbers num-2 num-5)
        num-6 (find-6 all-numbers num-5 num-9)
        num-0 (find-0 all-numbers num-6 num-9)]
    {num-1 "1"
     num-2 "2"
     num-3 "3"
     num-0 "0"
     num-4 "4"
     num-5 "5"
     num-6 "6"
     num-7 "7"
     num-8 "8"
     num-9 "9"}))

(defn decode [numbers number-map]
  (let [digits (map #(get number-map %) numbers)
        numstr (clojure.string/join digits)
        num (Integer/parseInt numstr)]
    num))

(defn extract-tokens [lines idx]
  (let [splitted (map #(clojure.string/split % #" \| ") lines)
        side (map #(nth % idx) splitted)
        strings (map #(clojure.string/split % #" ") side)
        strings-vec (apply concat strings)]
    strings-vec))

(defn oneof-1478 [token]
  (let [len (count token)]
    (or (= len 2) (= len 3) (= len 4) (= len 7))))

(defn task01 [lines]
  (let [signals (extract-tokens lines 1)
        signals-1478 (filter oneof-1478 signals)]
    (println "Number of (1|4|7|8):" (count signals-1478))))

(defn get-number-for-line [line]
  (let [sides (clojure.string/split line #" \| ")
        all-sets (vec (map set (clojure.string/split (first sides) #" ")))
        signal-sets (vec (map set (clojure.string/split (last sides) #" ")))
        num-map (fill-map all-sets)]
   (decode signal-sets num-map)))

(defn task02 [lines]
  (let [result (apply + (map get-number-for-line lines))]
    (println "Result Task 2:" result)))

(defn day08
  ([] (println "Using default input")
      (day08 "input_day_08.txt"))
  ([filename]
   (let [lines (read-lines filename)]
     (task01 lines)
     (task02 lines))))

(defn -main [] (day08))