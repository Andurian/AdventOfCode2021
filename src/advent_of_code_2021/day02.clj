(ns advent-of-code-2021.day02
  (:require [advent-of-code-2021.util :refer [read-lines]])
  (:gen-class))

(defn count [keyword tokens]
  (let [fn-filter (fn [pair] (= (pair 0) keyword))
        fn-map (fn [pair] (read-string (pair 1)))]
    (reduce + (map fn-map (filter fn-filter tokens)))))

(defn find-position [instructions]
  (let [tokens (map (fn [instruction] (clojure.string/split instruction #" ")) instructions)
        forward (count "forward" tokens)
        down (count "down" tokens)
        up (count "up" tokens)]

    [forward (- down up)]))

(defn find-position2 [instructions]
  (loop [pos [0 0]
         tokens (map (fn [instruction] (clojure.string/split instruction #" ")) instructions)]
    (if (empty? tokens) pos
        (recur (let [command ((first tokens) 0)
                     value (read-string ((first tokens) 1))]
                 (case command
                   "forward" [(+ (pos 0) value) (pos 1)]
                   "up" [(pos 0) (- (pos 1) value)]
                   "down" [(pos 0) (+ (pos 1) value)])) (rest tokens)))))

(defn find-position3 [instructions]
  (loop [pos [0 0 0]
         tokens (map (fn [instruction] (clojure.string/split instruction #" ")) instructions)]
    (if (empty? tokens) pos
        (recur (let [command ((first tokens) 0)
                     value (read-string ((first tokens) 1))]
                 (case command
                   "forward" [(+ (pos 0) value) (+ (pos 1) (* (pos 2) value)) (pos 2)]
                   "up" [(pos 0) (pos 1) (- (pos 2) value)]
                   "down" [(pos 0) (pos 1) (+ (pos 2) value)])) (rest tokens)))))

(defn day02
  ([] (println "Using default input")
      (day02 "day_02/input_01.txt"))
  ([filename]
   (let [lines (read-lines filename)
         pos (find-position3 lines)
        ]

     (println "Position is: [" (pos 0) "," (pos 1) "]")
     (println "Answer is:" (* (pos 0) (pos 1)))
     )))

(defn -main [] (day02))