(ns advent-of-code-2021.day05
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string]
            [clojure.set])
  (:gen-class))

(defn horizontal-coords [x1 x2 y]
  (vec (map #(vector % y) (range x1 (inc x2)))))

(defn vertical-coords [y1 y2 x]
  (vec (map #(vector x %) (range y1 (inc y2)))))

(defn diagonal-coords [x1 y1 x2 y2]
  (let [len (Math/abs (- x2 x1))
        dx (/ (- x2 x1) len)
        dy (/ (- y2 y1) len)]
    (vec (map #(vector (+ x1 (* % dx)) (+ y1 (* % dy))) (range (inc len))))))

(defn covered-squares [x1 y1 x2 y2]
  (cond
    (= x1 x2)
    (vertical-coords (min y1 y2) (max y1 y2) x1)
    (= y1 y2)
    (horizontal-coords (min x1 x2) (max x1 x2) y1)
    :else (diagonal-coords x1 y1 x2 y2)))

(defn parse-line [line]
  (let [[c1 c2] (clojure.string/split line #" -> ")
        [x1 y1] (clojure.string/split c1 #",")
        [x2 y2] (clojure.string/split c2 #",")]
    [(read-string x1) (read-string y1) (read-string x2) (read-string y2)]))

(defn add-line [current-map line]
  (let [squares (apply covered-squares (parse-line line))]
    (loop [m current-map i 0]
      (cond
        (>= i (count squares)) m
        :else (recur (assoc m (nth squares i) (inc (get m (nth squares i) 0))) (inc i))))))

(defn fill-map [lines]
  (loop [m {} i 0]
    (cond (>= i (count lines)) m
          :else (recur (add-line m (nth lines i)) (inc i)))))

(defn task01 [lines]
  (let [line-map (fill-map lines)]
    (->> line-map
         (filter #(>= (% 1) 2))
         (count)
         (println))))

(defn day05
  ([] (println "Using default input")
      (day05 "input_day_05.txt"))
  ([filename]
   (let [lines (read-lines filename)]
     (task01 lines);
     ;(task02 lines);
     )))

(defn -main [] (day05))