(ns advent-of-code-2021.day02
  (:require [advent-of-code-2021.util :as util]
            [clojure.string :as str])
  (:gen-class))

(defn- count-tokens [keyword tokens]
  (let [fn-filter (fn [pair] (= (pair 0) keyword))
        fn-map (fn [pair] (read-string (pair 1)))]
    (reduce + (map fn-map (filter fn-filter tokens)))))

(defn- find-position-task1 [instructions]
  (let [tokens (map  #(str/split % #" ") instructions)
        forward (count-tokens "forward" tokens)
        down (count-tokens "down" tokens)
        up (count-tokens "up" tokens)]

    [forward (- down up)]))

(defn- find-position-task1-alternative [instructions]
  (loop [pos [0 0]
         tokens (map  #(str/split % #" ") instructions)]
    (if (empty? tokens) pos
        (recur (let [command ((first tokens) 0)
                     value (read-string ((first tokens) 1))]
                 (case command
                   "forward" [(+ (pos 0) value) (pos 1)]
                   "up" [(pos 0) (- (pos 1) value)]
                   "down" [(pos 0) (+ (pos 1) value)])) (rest tokens)))))

(defn- find-position-task2 [instructions]
  (loop [pos [0 0 0]
         tokens (map  #(str/split % #" ") instructions)]
    (if (empty? tokens) pos
        (recur (let [command ((first tokens) 0)
                     value (read-string ((first tokens) 1))]
                 (case command
                   "forward" [(+ (pos 0) value) (+ (pos 1) (* (pos 2) value)) (pos 2)]
                   "up" [(pos 0) (pos 1) (- (pos 2) value)]
                   "down" [(pos 0) (pos 1) (+ (pos 2) value)])) (rest tokens)))))

(defn day02
  ([] (day02 "input_day_02.txt"))
  ([filename]
   (let [lines (util/read-lines filename)
         pos1a (find-position-task1 lines)
         pos1b (find-position-task1-alternative lines)
         pos2 (find-position-task2 lines)]
     (println (format "Solution Day 02-1a: %d (Position is %s)" (apply * pos1a) (str pos1a)))
     (println (format "Solution Day 02-1b: %d (Position is %s)" (apply * pos1b) (str pos1b)))
     (println (format "Solution Day 02-2:  %d (Position is %s)" (* (pos2 0) (pos2 1)) (str pos2))))))

(defn -main [] (time (day02)))