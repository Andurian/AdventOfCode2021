(ns advent-of-code-2021.day05
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string]
            [clojure.set])
  (:gen-class))

(defn- horizontal-coords [x1 x2 y]
  (vec (map #(vector % y) (range x1 (inc x2)))))

(defn- vertical-coords [y1 y2 x]
  (vec (map #(vector x %) (range y1 (inc y2)))))

(defn- diagonal-coords [x1 y1 x2 y2]
  (let [len (Math/abs (- x2 x1))
        dx (/ (- x2 x1) len)
        dy (/ (- y2 y1) len)]
    (vec (map #(vector (+ x1 (* % dx)) (+ y1 (* % dy))) (range (inc len))))))

(defn- ignore-diagonal-coords [_ _ _ _]
  [])

(defn- covered-squares [[x1 y1 x2 y2] fn-diagonal-coords]
  (cond
    (= x1 x2)
    (vertical-coords (min y1 y2) (max y1 y2) x1)
    (= y1 y2)
    (horizontal-coords (min x1 x2) (max x1 x2) y1)
    :else (fn-diagonal-coords x1 y1 x2 y2)))

(defn- parse-line [line]
  (let [[c1 c2] (clojure.string/split line #" -> ")
        [x1 y1] (clojure.string/split c1 #",")
        [x2 y2] (clojure.string/split c2 #",")]
    [(read-string x1) (read-string y1) (read-string x2) (read-string y2)]))

(defn- add-line [current-map line fn-diagonal-coords]
  (let [squares (covered-squares (parse-line line) fn-diagonal-coords)]
    (loop [m current-map i 0]
      (cond
        (>= i (count squares)) m
        :else (recur (assoc m (nth squares i) (inc (get m (nth squares i) 0))) (inc i))))))

(defn- fill-map [lines fn-diagonal-coords]
  (loop [m {} i 0]
    (cond (>= i (count lines)) m
          :else (recur (add-line m (nth lines i) fn-diagonal-coords) (inc i)))))

(defn- task [lines fn-diagonal-coords]
  (let [line-map (fill-map lines fn-diagonal-coords)]
    (->> line-map
         (filter #(>= (% 1) 2))
         (count))))

(defn day05
  ([] (day05 "input_day_05.txt"))
  ([filename]
   (let [lines (read-lines filename)]
     (println "Solution Day 05-1:" (task lines ignore-diagonal-coords))
     (println "Solution Day 05-2:" (task lines diagonal-coords)))))

(defn -main [] (time (day05)))