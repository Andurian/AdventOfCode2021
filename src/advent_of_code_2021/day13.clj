(ns advent-of-code-2021.day13
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string])
  (:gen-class))

(defn- ->coordinates [line]
  (let [tokens (clojure.string/split line #",")]
    (vec (map #(Integer/parseInt %) tokens))))

(defn- ->instruction [line]
  (let [axis-str (last (clojure.string/split line #" "))
        tokens (clojure.string/split axis-str #"=")]
    {:axis (first tokens) :value (Integer/parseInt (last tokens))}))

(defn- print-points [points]
  (let [max-x (apply max (map first points))
        max-y (apply max (map last points))
        cols (inc max-x)
        rows (inc max-y)
        repr (loop [current (vec (repeat (* cols rows) "."))
                    i 0]
               (if (>= i (count points))
                 current
                 (let [p (nth points i)
                       x (first p)
                       y (last p)
                       idx (+ x (* cols y))]
                   (recur (assoc current idx "*") (inc i)))))]
    (println (clojure.string/join (apply concat (interpose "\n" (partition cols repr)))))))

(defn- parse-input [lines]
  (let [coordinate-lines (take-while #(not (= "" %)) lines)
        instruction-lines (take-last (- (count lines) (+ 1 (count coordinate-lines))) lines)
        coordinates (map ->coordinates coordinate-lines)
        instructions (map ->instruction instruction-lines)]
    {:points (vec coordinates) :instructions (vec instructions)}))

(defn- fold [points instruction]
  (let [ax (if (= (:axis instruction) "x") 0 1)
        mirror-at (:value instruction)]
    (loop [current-points points
           i 0]
      (if (>= i (count current-points))
        current-points
        (let [current-point (nth current-points i)
              relevant-value (nth current-point ax)
              mirrored-point (if (> relevant-value mirror-at)
                               (assoc current-point ax (- (* 2 mirror-at) relevant-value))
                               current-point)]
          (recur (vec (assoc current-points i mirrored-point)) (inc i)))))))

(defn- fold-all [input]
  (loop [current-points (:points input)
         i 0]
    (if (>= i (count (:instructions input)))
      current-points
      (recur (fold current-points (nth (:instructions input) i)) (inc i)))))

(defn- task01 [lines]
  (let [input (parse-input lines)
        p1 (fold (:points input) (first (:instructions input)))]
    (println "Solution Day 13-1:" (count (set p1)))))

(defn- task02 [lines]
  (let [input (parse-input lines)
        result (fold-all input)]
    (println "Solution Day 13-2:")
    (print-points result)))

(defn day13
  ([] (day13 "input_day_13.txt"))
  ([filename]
   (let [lines (read-lines filename)]
     (task01 lines)
     (task02 lines))))

(defn -main [] (time (day13)))