(ns advent-of-code-2021.day03
  (:require [advent-of-code-2021.util :refer [read-lines]])
  (:gen-class))

(defn transpose [m]
  (apply mapv vector m))

(defn occurrence [bits cmp]
  (let [freq (frequencies bits)]
    (if (cmp (get freq "0") (get freq "1")) "0" "1")))

(defn read-binary [bits]
  (read-string (clojure.string/join ["2r" (clojure.string/join bits)])))

(defn invert [bits]
  (map #(if (= % "1") "0" "1") bits))

(defn task01 [bit-matrix]
  (let [bits (map #(occurrence % >) (transpose bit-matrix))
        gamma-rate (read-binary bits)
        epsilon-rate (read-binary (invert bits))]
    (println "Task 01:");
    (println "\tGamma Rate:" gamma-rate);
    (println "\tEpsilon Rate:" epsilon-rate);
    (println "\tPower Consumption:" (* epsilon-rate gamma-rate) "\n");
    ))

(defn filter-rows [bit-matrix pos cmp]
  (let [target (occurrence (get (transpose bit-matrix) pos) cmp)]
    (filter #(= (get % pos) target) bit-matrix)))

(defn life-support-ratings [bit-matrix cmp]
  (loop [rows bit-matrix pos 0]
    (if (= (count rows) 1) (first rows)
        (recur (filter-rows rows pos cmp) (+ pos 1)))))

(defn task02 [bit-matrix]
  (let [oxygen-rating (read-binary (life-support-ratings bit-matrix >))
        co2-rating (read-binary (life-support-ratings bit-matrix <=))]
    (println "Task 02:");
    (println "\tOxygen Generator Rating:" oxygen-rating);
    (println "\tCO2 Scrubber Rating:" co2-rating);
    (println "\tAnswer: " (* oxygen-rating co2-rating));
    ))

(defn day03
  ([] (println "Using default input")
      (day03 "day_03/input_01.txt"))
  ([filename]
   (let [bit-matrix (map #(clojure.string/split % #"") (read-lines filename))]

     (task01 bit-matrix);
     (task02 bit-matrix);
     )))

(defn -main [] (day03))