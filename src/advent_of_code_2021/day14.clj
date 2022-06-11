(ns advent-of-code-2021.day14
  (:require [advent-of-code-2021.util :refer [read-lines get-time]]
            [clojure.string])
  (:gen-class))

(defn- single-rule [[l r]]
  (let [p1 (first l)
        p2 (last l)]
    [l [(clojure.string/join [p1 r])
        (clojure.string/join [r p2])]]))

(defn- to-map [pattern]
  (let [parts (partition 2 1 pattern)]
    (loop [current {}
           i 0]
      (if (>= i (count parts))
        current
        (let [k (clojure.string/join (nth parts i))
              v (inc (get current k 0))]
          (recur (assoc current k v) (inc i)))))))

(defn- parse-input-map [lines]
  (let [start (first lines)
        rules-lines (take-last (- (count lines) 2) lines)
        rules (into {} (map #(single-rule (clojure.string/split % #" -> ")) rules-lines))]
    {:start (to-map start) :rules rules :c1 (first start) :c2 (last start)}))

(defn- parse-input [lines]
  (let [start (first lines)
        rules-lines (take-last (- (count lines) 2) lines)
        rules (into {} (map #(clojure.string/split % #" -> ") rules-lines))]
    {:start start :rules rules}))

(defn- step [input rules]
  (let [to-insert (vec (map #(get rules (clojure.string/join %)) (partition 2 1 input)))]
    (clojure.string/join (cons (first input) (interleave to-insert (rest input))))))

(defn- step-map [input rules]
  (let [input-seq (seq input)]
    (loop [current {} i 0]
      (if (>= i (count input-seq))
        current
        (let [k (first (nth input-seq i))
              v (last (nth input-seq i))
              [p1 p2] (get rules k)
              new-v1 (+ v (get current p1 0))
              new-v2 (+ v (get current p2 0))]
          (recur (assoc (assoc current p1 new-v1) p2 new-v2) (inc i)))))))

(defn- transform-map [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn- frequencies-map [input c1 c2]
  (let [input-seq (seq input)
        intermediate (loop [current {c1 1 c2 1}
                            i 0]
                       (if (>= i (count input-seq))
                         current
                         (let [current-val (nth input-seq i)
                               [p1 p2] (first current-val)
                               v (bigint (last current-val))
                               v1 (+ v (get current p1 (bigint 0)))
                               v2 (+ v (get current p2 (bigint 0)))
                               new-map (if (= p1 p2)
                                         (assoc current p1 (+ (get current p1 0) v v))
                                         (assoc (assoc current p1 v1) p2 v2))]
                           (recur new-map (inc i)))))]
    (transform-map intermediate #(/ % 2))))

(defn- step-n [input rules n]
  (loop [current input
         i 0]
    (if (>= i n)
      current
      (recur (step current rules) (inc i)))))

(defn- step-map-n [input rules n]
  (loop [current input
         i 0]
    (if (>= i n)
      current
      (recur (step-map current rules) (inc i)))))

(defn- task01 [lines]
  (let [input (parse-input lines)
        result (step-n (:start input) (:rules input) 10)
        freq (vals (frequencies result))]
    (- (apply max freq) (apply min freq))))

(defn- task01-map [lines]
  (let [input (parse-input-map lines)
        result (step-map-n (:start input) (:rules input) 10)
        freq-map (frequencies-map result (:c1 input) (:c2 input))
        freq (vals freq-map)]
    (- (apply max freq) (apply min freq))))


(defn- task02 [lines]
  (let [input (parse-input-map lines)
        result (step-map-n (:start input) (:rules input) 40)
        freq-map (frequencies-map result (:c1 input) (:c2 input))
        freq (vals freq-map)]
    (- (apply max freq) (apply min freq))))

(defn day14
  ([] (day14 "input_day_14.txt"))
  ([filename]
   (let [lines (read-lines filename)
         [time1a res1a] (get-time (task01 lines))
         [time1b res1b] (get-time (task01-map lines))
         [time2 res2] (get-time (task02 lines))]
     (println (format "Solution Day 14-1a: %s (Time %f ms)" (str res1a) time1a))
     (println (format "Solution Day 14-1b: %s (Time %f ms)" (str res1b) time1b))
     (println (format "Solution Day 14-2:  %s (Time %f ms)" (str res2) time2)))))

(defn -main [] (time (day14)))