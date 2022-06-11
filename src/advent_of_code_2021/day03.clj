(ns advent-of-code-2021.day03
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string])
  (:gen-class))

(defn- transpose [m]
  (apply mapv vector m))

(defn- occurrence [bits cmp]
  (let [freq (frequencies bits)]
    (if (cmp (get freq "0") (get freq "1")) "0" "1")))

(defn- read-binary [bits]
  (read-string (clojure.string/join ["2r" (clojure.string/join bits)])))

(defn- invert [bits]
  (map #(if (= % "1") "0" "1") bits))

(defn- task01 [bit-matrix]
  (let [bits (map #(occurrence % >) (transpose bit-matrix))
        gamma-rate (read-binary bits)
        epsilon-rate (read-binary (invert bits))]
    (println (format "Solution Day 03-1: %d (Gamma Rate: %d, Epsilon Rate: %d)"
                     (* epsilon-rate gamma-rate)
                     gamma-rate
                     epsilon-rate))))

(defn- filter-rows [bit-matrix pos cmp]
  (let [target (occurrence (get (transpose bit-matrix) pos) cmp)]
    (filter #(= (get % pos) target) bit-matrix)))

(defn- life-support-ratings [bit-matrix cmp]
  (loop [rows bit-matrix pos 0]
    (if (= (count rows) 1) (first rows)
        (recur (filter-rows rows pos cmp) (+ pos 1)))))

(defn- task02 [bit-matrix]
  (let [oxygen-rating (read-binary (life-support-ratings bit-matrix >))
        co2-rating (read-binary (life-support-ratings bit-matrix <=))]
    (println (format "Solution Day 03-2: %d (Oxygen Generator Rating: %d, CO2 Scrubber Rating: %d)"
                     (* oxygen-rating co2-rating)
                     oxygen-rating
                     co2-rating))))

(defn day03
  ([] (day03 "input_day_03.txt"))
  ([filename]
   (let [bit-matrix (map #(clojure.string/split % #"") (read-lines filename))]
     (task01 bit-matrix)
     (task02 bit-matrix))))

(defn -main [] (time (day03)))