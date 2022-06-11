(ns advent-of-code-2021.day06
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string])
  (:gen-class))

(defn- step-vec [ages]
  (let [num-new-fishes (count (filter #(= % 0) ages))
        updatet-fishes (vec (map #(if (= % 0) 6 (dec %)) ages))
        new-fishes (repeat num-new-fishes 8)]
    (vec (concat updatet-fishes new-fishes))))

(defn- evolve [ages, num-days, fn-step]
  (loop [current-ages ages remaining-days num-days]
    (cond
      (= 0 remaining-days)
      current-ages
      :else
      (recur (fn-step current-ages) (dec remaining-days)))))

(defn- to-map [ages]
  (loop [m {} d 0]
    (cond
      (> d 8)
      m
      :else
      (recur (assoc m d (count (filter #(= % d) ages))) (inc d)))))

(defn- step-map [age-map]
  (loop [m {} d 8]
    (cond
      (> d 0)
      (recur (assoc m (dec d) (get age-map d)) (dec d))
      (= d 0)
      (assoc (assoc m 8 (get age-map 0)) 6 (+ (get m 6) (get age-map 0))))))

(defn- count-map [age-map]
  (reduce + (map #(% 1) age-map)))

(defn- task01 [ages]
  (let [fishes (evolve ages 80 step-vec)]
    (println "Solution Day 06-1:" (count fishes))))

(defn- task02 [ages]
  (let [fishes (evolve (to-map ages) 256 step-map)]
    (println "Solution Day 06-2:" (count-map fishes))))

(defn day06
  ([] (day06 "input_day_06.txt"))
  ([filename]
   (let [lines (read-lines filename)
         line (first lines)
         tokens (vec (map read-string (clojure.string/split line #",")))]
     (task01 tokens)
     (task02 tokens))))

(defn -main [] (time (day06)))