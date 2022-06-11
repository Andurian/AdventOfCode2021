(ns advent-of-code-2021.day07
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string])
  (:gen-class))

(defn- median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (quot (+ bottom-val top-val) 2)))))

(defn- fuel-cost-const [positions target]
  (reduce + (map #(Math/abs (- % target)) positions)))

(defn- sum-to-n [n]
  (apply + (range 1 (inc n))))

(defn- fuel-cost-acc [positions target]
  (reduce + (map #(sum-to-n (Math/abs (- % target))) positions)))

(defn- task01 [positions]
  (let [target (median positions)
        cost (fuel-cost-const positions target)]
    (println (format "Solution Day 07-1: %d (Target at %d)" cost target))))

(defn- task02 [positions]
  (let [val (apply min (map #(fuel-cost-acc positions %) (range 1000)))]
    (println "Solution Day 07-2:" val)))

(defn day07
  ([] (day07 "input_day_07.txt"))
  ([filename]
   (let [lines (read-lines filename)
         tokens (vec (map read-string (clojure.string/split (first lines) #",")))]
     (task01 tokens)
     (task02 tokens))))

(defn -main [] (time (day07)))