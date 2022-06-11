(ns advent-of-code-2021.day25
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string :as str]
            [clojure.set])
  (:gen-class))

(defn- to-field [lines]
  (vec (map vec lines)))

(defn- all-coords
  ([field] (all-coords (count field) (count (first field))))
  ([rows cols]
   (for [r (range rows) c (range cols)] (vector r c))))

(defn- create-map [lines]
  (let [field (to-field lines)
        coords (all-coords field)
        state (into {} (map (fn [[r c]] [[r c] (nth (nth field r) c)]) coords))]
    {:state state
     :rows (count field)
     :cols (count (first field))}))

(defn- create-set [{:keys [state rows cols]}]
  (let [get-set (fn [content] (set (map first (filter (fn [[_ v]] (= content v)) state))))]
    {:to-east (get-set \>)
     :to-south (get-set \v)
     :rows rows
     :cols cols}))

(defn- get-from-set [{:keys [to-east to-south]} index]
  (cond
    (contains? to-east index) \>
    (contains? to-south index) \v
    :else \.))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- print-field-set [{:keys [rows cols] :as all}]
  (let [chars (for [r (range rows)
                    c (range cols)]
                (get-from-set all [r c]))]
    (println (clojure.string/join \newline (map #(clojure.string/join "" %) (partition cols chars))))))

(defn- east-neighbor-coord [[r c] {:keys [cols]}]
  (if (>= c (dec cols)) [r 0] [r (inc c)]))

(defn- south-neighbor-coord [[r c] {:keys [rows]}]
  (if (>= r (dec rows)) [0 c] [(inc r) c]))

(defn- east-moving-neighbors-set [{:keys [to-east] :as all}]
  (let [movable (set (filter #(= \. (get-from-set all (east-neighbor-coord % all))) to-east))
        neighbors (set (map #(east-neighbor-coord % all) movable))]
    [movable neighbors]))

(defn- south-moving-neighbors-set [{:keys [to-south] :as all}]
  (let [movable (set (filter #(= \. (get-from-set all (south-neighbor-coord % all))) to-south))
        neighbors (set (map #(south-neighbor-coord % all) movable))]
    [movable neighbors]))

(defn- make-single-move-set [state key [movable neighbors]]
  (let [changing (get state key)
        temp1 (clojure.set/difference changing movable)
        temp2 (clojure.set/union temp1 neighbors)]
    (assoc state key temp2)))

(defn- make-move-set [state]
  (let [move-east (east-moving-neighbors-set state)
        east-moved-state (make-single-move-set state :to-east move-east)
        move-south (south-moving-neighbors-set east-moved-state)]
    (make-single-move-set east-moved-state :to-south move-south)))

(defn- run [initial-state step-fn]
  (loop [i 1
         old-state initial-state
         new-state (step-fn initial-state)]
    (if (= old-state new-state)
        i
      (recur (inc i) new-state (step-fn new-state)))))

(defn day25 []
  (let [input (read-lines "input_day_25.txt")
        lines (to-field input)
        state (create-set (create-map lines))]
    (println "Solution Day 25:" (run state make-move-set))))

(defn -main [] (time (day25)))