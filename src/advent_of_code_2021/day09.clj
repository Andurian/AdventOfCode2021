(ns advent-of-code-2021.day09
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string]
            [clojure.set])
  (:gen-class))

(defn to-field [lines]
  (vec (map (fn [line] (vec (map #(Integer/parseInt (Character/toString %)) line))) lines)))

(defn all-coords [rows cols]
  (for [r (range rows) c (range cols)] (vector r c)))

(defn get-value
  ([coords field] (get-value (coords 0) (coords 1) field))
  ([r c field]
   (let [rows (count field)
         cols (count (field 0))]
     (cond
       (< r 0) nil
       (>= r rows) nil
       (< c 0) nil
       (>= c cols) nil
       :else ((field r) c)))))

(defn neighbor-coords
  ([coords field] (neighbor-coords (coords 0) (coords 1) field))
  ([r c field]
   (let [coords [[(dec r) c] [r (dec c)] [r (inc c)] [(inc r) c]]]
     (filter #(not (nil? (get-value % field))) coords))))

(defn neighbors
  ([coords field] (neighbors (coords 0) (coords 1) field))
  ([r c field]
   (map #(get-value % field) (neighbor-coords r c field))))

(defn min? [coords field]
  (let [val (get-value coords field)
        neighb (neighbors coords field)]
    (= (count neighb) (count (filter #(= true %) (map #(< val %) neighb))))))

(defn basin-size [min-coords field]
  (loop [visited #{}
         to-visit #{min-coords}]
    (cond
      (empty? to-visit) (count visited)
      :else
      (let [current (first to-visit)
            remaining (rest to-visit)
            candidates (filter #(and (< (get-value % field) 9) (not (contains? visited %))) (neighbor-coords current field))]
        (recur (into visited #{current}) (into remaining candidates))))))


(defn task01 [lines]
  (let [field (to-field lines)
        rows (count field)
        cols (count (field 0))
        coords (all-coords rows cols)
        minima (filter #(min? % field) coords)
        min-vals (map #(get-value % field) minima)
        risk-value (apply + (map inc min-vals))]
    (println "Risk value:" risk-value)))

(defn task02 [lines]
  (let [field (to-field lines)
        rows (count field)
        cols (count (field 0))
        result (->> (all-coords rows cols)
                    (filter #(min? % field))
                    (map #(basin-size % field))
                    (sort >)
                    (take 3)
                    (apply *))]
    (println "Product of three larges basins:" result)))

(defn day09
  ([] (println "Using default input")
      (day09 "input_day_09.txt"))
  ([filename]
   (let [lines (read-lines filename)]
     (task01 lines);
     (task02 lines);
     )))

(defn -main [] (day09))