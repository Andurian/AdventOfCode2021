(ns advent-of-code-2021.day25
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string :as str]
            [clojure.set])
  (:gen-class))

(defn to-field [lines]
  (vec (map vec lines)))

(defn all-coords
  ([field] (all-coords (count field) (count (first field))))
  ([rows cols]
   (for [r (range rows) c (range cols)] (vector r c))))

(defn create-map [lines]
  (let [field (to-field lines)
        coords (all-coords field)
        state (into {} (map (fn [[r c]] [[r c] (nth (nth field r) c)]) coords))]
    {:state state
     :rows (count field)
     :cols (count (first field))}))

(defn create-set [{:keys [state rows cols]}]
  (let [get-set (fn [content] (set (map first (filter (fn [[_ v]] (= content v)) state))))]
    {:to-east (get-set \>)
     :to-south (get-set \v)
     :rows rows
     :cols cols}))

(defn get-from-set [{:keys [to-east to-south]} index]
  (cond
    (contains? to-east index) \>
    (contains? to-south index) \v
    :else \.))

(defn print-field [{:keys [state rows cols]}]
  (let [chars (for [r (range rows)
                    c (range cols)]
                (get state [r c]))]
    (println (clojure.string/join \newline (map #(clojure.string/join "" %) (partition cols chars))))))

(defn print-field-set [{:keys [rows cols] :as all}]
  (let [chars (for [r (range rows)
                    c (range cols)]
                (get-from-set all [r c]))]
    (println (clojure.string/join \newline (map #(clojure.string/join "" %) (partition cols chars))))))

(defn east-neighbor-coord [[r c] {:keys [cols]}]
  (if (>= c (dec cols)) [r 0] [r (inc c)]))

(defn south-neighbor-coord [[r c] {:keys [rows]}]
  (if (>= r (dec rows)) [0 c] [(inc r) c]))

(defn find-coords [state val]
  (map first (filter (fn [[_ v]] (= v val)) state)))

(defn east-moving-neighbors [{:keys [state] :as all}]
  (let [coords (find-coords state \>)
        neighbors (map #(east-neighbor-coord % all) coords)
        free-indices (filter #(not (= -1 %)) (map-indexed (fn [idx v] (if (= \. (get state v)) idx -1)) neighbors))
        free-neighbors (keep-indexed (fn [idx v] (if (some #(= idx %) free-indices) v nil)) neighbors)
        free-coords (keep-indexed (fn [idx v] (if (some #(= idx %) free-indices) v nil)) coords)]
    [free-coords free-neighbors]))

(defn east-moving-neighbors-set [{:keys [to-east] :as all}]
  (let [movable (set (filter #(= \. (get-from-set all (east-neighbor-coord % all))) to-east))
        neighbors (set (map #(east-neighbor-coord % all) movable))]
    [movable neighbors]))

(defn south-moving-neighbors-set [{:keys [to-south] :as all}]
  (let [movable (set (filter #(= \. (get-from-set all (south-neighbor-coord % all))) to-south))
        neighbors (set (map #(south-neighbor-coord % all) movable))]
    [movable neighbors]))

(defn south-moving-neighbors [{:keys [state] :as all}]
  (let [coords (find-coords state \v)
        neighbors (map #(south-neighbor-coord % all) coords)
        free-indices (filter #(not (= -1 %)) (map-indexed (fn [idx v] (if (= \. (get state v)) idx -1)) neighbors))
        free-neighbors (keep-indexed (fn [idx v] (if (some #(= idx %) free-indices) v nil)) neighbors)
        free-coords (keep-indexed (fn [idx v] (if (some #(= idx %) free-indices) v nil)) coords)]
    [free-coords free-neighbors]))

(defn make-single-move-set [state key [movable neighbors]]
  (let [changing (get state key)
        temp1 (clojure.set/difference changing movable)
        temp2 (clojure.set/union temp1 neighbors)]
    (assoc state key temp2)))

(defn make-single-move [{:keys [state] :as all} [old new]]
  (loop [i 0
         current state]
    (if (>= i (count old))
      (assoc all :state current)
      (let [old-i (nth old i)
            new-i (nth new i)
            content (get current old-i)]
        (recur (inc i) (assoc (assoc current old-i \.) new-i content))))))

(defn make-move-set [state]
  (let [move-east (east-moving-neighbors-set state)
        east-moved-state (make-single-move-set state :to-east move-east)
        move-south (south-moving-neighbors-set east-moved-state)]
    (make-single-move-set east-moved-state :to-south move-south)))

(defn make-move [state]
  (let [move-east (east-moving-neighbors state)
        east-moved-state (make-single-move state move-east)
        move-south (south-moving-neighbors east-moved-state)]
    (make-single-move east-moved-state move-south)))

(defn run [initial-state step-fn print-fn]
  (loop [i 1
         old-state initial-state
         new-state (step-fn initial-state)]
    (println "Finished step" i)
    (if (= old-state new-state)
      (do
        (print-fn new-state)
        (println "Finished after" i "steps."))
      (recur (inc i) new-state (step-fn new-state)))))

(defn day25 []
  (let [input (read-lines "input_day_25.txt");_test_02.txt")
        lines (to-field input)
        state (create-map lines)]
    (run state make-move print-field);
    ))

(defn day25-simplified []
  (let [input (read-lines "input_day_25.txt");_test_02.txt")
        lines (to-field input)
        state (create-set (create-map lines))]
    (time (run state make-move-set print-field-set))))

(defn -main [] (day25-simplified))