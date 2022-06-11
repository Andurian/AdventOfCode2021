(ns advent-of-code-2021.day01
  (:require [advent-of-code-2021.util :refer [read-numbers]])
  (:gen-class))

(defn- window [values start size]
  (let [indices (range start (+ start size))]
    (reduce + (map #(values %) indices))))

(defn- count-increasing [values window-size]
  (let [indices (range (- (count values) window-size))
        get-window (fn [start] (window values start window-size))
        counter (fn [index] (let [left (get-window index)
                                  right (get-window (+ index 1))]
                              (if (< left right) 1 0)))]
    (reduce + (map counter indices))))

(defn day01
  ([] (day01 "input_day_01.txt"))
  ([filename]
   (let [measurements (read-numbers filename)]
     (println "Solution Day 01-1:" (count-increasing measurements 1))
     (println "Solution Day 01-2:" (count-increasing measurements 3)))))

(defn -main [] (time (day01)))