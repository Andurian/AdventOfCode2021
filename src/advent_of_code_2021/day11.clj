(ns advent-of-code-2021.day11
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string]
            [clojure.set])
  (:gen-class))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- print-field [field]
  (let [val-to-char (fn [value] (cond
                                  (= :default (:status value)) (str (:level value))
                                  (= :flashing (:status value)) "*"
                                  :else "/"))
        row-to-str (fn [row] (clojure.string/join " " (map val-to-char row)))]
    (println (clojure.string/join  "\n" (map row-to-str field)))))

(defn- get-value
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

(defn- to-matrix [lines]
  (let [to-map-obj (fn [value] {:level value :status :default})
        parse-line (fn [line] (vec (map #(to-map-obj (Integer/parseInt (Character/toString %))) line)))]
    (vec (map parse-line lines))))

(defn- all-coords
  ([field] (all-coords (count field) (count (field 0))))
  ([rows cols] (vec (map vec (partition cols (for [r (range rows) c (range cols)] (vector r c)))))))

(defn- neighbor-coords
  ([coords field] (neighbor-coords (coords 0) (coords 1) field))
  ([r c field]
   (let [coords [[(dec r) (dec c)]
                 [(dec r) c]
                 [(dec r) (inc c)]
                 [r (dec c)]
                 [r (inc c)]
                 [(inc r) (dec c)]
                 [(inc r) c]
                 [(inc r) (inc c)]]]
     (filter #(not (nil? (get-value % field))) coords))))

(defn- neighbors
  ([coords field] (neighbors (coords 0) (coords 1) field))
  ([r c field]
   (map #(get-value % field) (neighbor-coords r c field))))

(defn- step [func field]
  (let [rows (count field)
        cols (count (field 0))
        coords (all-coords field)
        step-row (fn [field-row coords-row]
                   (vec (map #(func (field-row %) (neighbors (coords-row %) field)) (range cols))))]
    (vec (map #(step-row (field %) (coords %)) (range rows)))))

(defn- stepfn-increase [value _]
  (if (= 9 (:level value))
    {:level 0 :status :flashing}
    {:level (+ 1 (:level value)) :status :default}))

(defn- stepfn-propagate-flash [value neighbors]
  (cond
    (= :flashed (:status value)) value
    (= :flashing (:status value)) (assoc value :status :flashed)
    :else
    (let [neighbor-flashes (count (filter #(= :flashing (:status %)) neighbors))
          new-level (+ (:level value) neighbor-flashes)]
      (if (>= new-level 10)
        {:level 0 :status :flashing}
        {:level new-level :status :default}))))

(defn- stepfn-return-to-default [value _]
  (assoc value :status :default))

(defn- count-in-field [field ff]
  (let [check-row (fn [row]
                    (reduce + (map #(if (ff (:status %)) 1 0) row)))]
    (reduce + (map check-row field))))


(defn- any-flashing? [field]
  (> (count-in-field field #(= :flashing %)) 0))

(defn- step-full [field]
  (let [increased-field (step stepfn-increase field)]
    (loop [current increased-field]
      (cond
        (not (any-flashing? current))
        {:field (step stepfn-return-to-default current) :flashes (count-in-field current #(= :flashed %))}
        :else
        (recur (step stepfn-propagate-flash current))))))

(defn- task01 [lines]
  (let [field (to-matrix lines)]
    (loop [idx 0
           current field
           num-flashes 0]
      (if (>= idx 100)
        num-flashes
        (let [step-result (step-full current)]
          (recur (inc idx) (:field step-result) (+ num-flashes (:flashes step-result))))))))

(defn- task02 [lines]
  (let [field (to-matrix lines)]
    (loop [idx 1
           current field]
      (let [step-result (step-full current)
            new-field (:field step-result)
            num-flashes (:flashes step-result)]
        (if (= 100 num-flashes)
          idx
          (recur (inc idx) new-field))))))

(defn day11
  ([] (day11 "input_day_11.txt"))
  ([filename]
   (let [lines (read-lines filename)]
     (println "Solution Day 11-1:" (task01 lines))
     (println "Solution Day 11-2:" (task02 lines)))))

(defn -main [] (time (day11)))