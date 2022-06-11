(ns advent-of-code-2021.day07
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string])
  (:gen-class))

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (Math/ceil (/ (double sum) (double count)))))

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (quot (+ bottom-val top-val) 2)))))

(defn fuel-cost-const [positions target]
  (reduce + (map #(Math/abs (- % target)) positions)))

(defn sum-to-n [n]
  (apply + (range 1 (inc n))))

(defn fuel-cost-acc [positions target]
  (reduce + (map #(sum-to-n (Math/abs (- % target))) positions)))

(defn task [positions fn-target fn-cost]
  (let [target (fn-target positions)
        cost (fn-cost positions target)]
    (println target)
    (println "Total cost to reach ideal position" target ":" cost)))

(defn task01 [positions]
  (task positions median fuel-cost-const))

(defn task02 [positions]
  (task positions mean fuel-cost-acc))

(defn task02a [positions]
  (let [val (apply min (map #(fuel-cost-acc positions %) (range 1000)))]
    (println val)))

(defn day07
  ([] (println "Using default input")
      (day07 "input_day_07.txt"))
  ([filename]
   (let [lines (read-lines filename)
         tokens (vec (map read-string (clojure.string/split (first lines) #",")))]
     (task01 tokens);
     (task02a tokens);
     )))

(defn -main [] (day07))