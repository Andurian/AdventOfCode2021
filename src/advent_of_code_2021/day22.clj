(ns advent-of-code-2021.day22
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string]
            [clojure.set])
  (:gen-class))

;; Parsing input
(defn parse-line [line]
  (let [[instruction coordinates] (clojure.string/split line #" ")
        coord-tokens (clojure.string/split coordinates #",")
        parse-coord (fn [token] (let [strs (clojure.string/split (clojure.string/join (drop 2 token)) #"\.\.")]
                                  (vec (map #(Integer/parseInt %) strs))))]
    {:instruction (if (= "on" instruction) :on :off)
     :x (parse-coord (nth coord-tokens 0))
     :y (parse-coord (nth coord-tokens 1))
     :z (parse-coord (nth coord-tokens 2))}))

(defn parse-input [lines]
  (vec (map parse-line lines)))

;; Task 1
(defn ->set [instruction]
  (let [limit (fn [[a b]] [(max a -50) (min b 50)])
        [x1 x2] (limit (:x instruction))
        [y1 y2] (limit (:y instruction))
        [z1 z2] (limit (:z instruction))]
    (loop [current #{}
           x x1
           y y1
           z z1]
      (cond
        (> z z2) current
        (> y y2) (recur current x1 y1 (inc z))
        (> x x2) (recur current x1 (inc y) z)
        :else (recur (conj current [x y z]) (inc x) y z)))))

(defn build-naive [instructions]
  (loop [current #{}
         to-process instructions]
    (cond
      (empty? to-process) current
      :else (let [i (:instruction (first to-process))
                  s (->set (first to-process))
                  f (if (= :on i) clojure.set/union clojure.set/difference)]
              (recur (f current s) (rest to-process))))))

(defn task-01 [instructions]
  (let [result (build-naive instructions)]
    (println (count result))))

;; Task 2
(defn intersection [old-cube new-cube]
  (let [state (if (= :off (:instruction old-cube)) :on :off)
        x [(max ((:x old-cube) 0) ((:x new-cube) 0))
           (min ((:x old-cube) 1) ((:x new-cube) 1))]
        y [(max ((:y old-cube) 0) ((:y new-cube) 0))
           (min ((:y old-cube) 1) ((:y new-cube) 1))]
        z [(max ((:z old-cube) 0) ((:z new-cube) 0))
           (min ((:z old-cube) 1) ((:z new-cube) 1))]
        candidate {:instruction state :x x :y y :z z}]
    (if (and (>= (x 1) (x 0))
             (>= (y 1) (y 0))
             (>= (z 1) (z 0)))
      candidate nil)))

(defn what-to-add [cubes new-cube]
  (loop [to-process cubes
         to-add (if (= :on (:instruction new-cube)) [new-cube] [])]
    (if (empty? to-process)
      to-add
      (let [inter (intersection (first to-process) new-cube)]
        (recur (rest to-process) (if (nil? inter) to-add (conj to-add inter)))))))

(defn build-as-set [instructions]
  (loop [cubes []
         to-process instructions]
    (if (empty? to-process)
      cubes
      (recur (concat cubes (what-to-add cubes (first to-process))) (rest to-process)))))

(defn count-cubes [cube]
  (let [factor (if (= :on (:instruction cube)) 1 -1)
        x (:x cube)
        y (:y cube)
        z (:z cube)]
    (* factor
       (inc (- (x 1) (x 0)))
       (inc (- (y 1) (y 0)))
       (inc (- (z 1) (z 0))))))

(defn count-all-cubes [cubes]
  (apply + (map count-cubes cubes)))

(defn task-02 [instructions]
  (let [result (build-as-set instructions)]
    (println (count-all-cubes result));
    ))

;; Task 1 in a more clever way
(defn filter-at [instructions limit]
  (let [coord-ok (fn [v] (and
                          (< (Math/abs (v 0)) limit)
                          (< (Math/abs (v 1)) limit)))
        cube-ok (fn [cube] (and (coord-ok (:x cube))
                                (coord-ok (:y cube))
                                (coord-ok (:z cube))))]
    (filter cube-ok instructions)))

(defn task-01-as-set [instructions]
  (let [input (filter-at instructions 50)
        result (build-as-set input)]
    (println (count-all-cubes result));
    ))

(defn day22
  ([] (println "Using default input")
      (day22 "input_day_22.txt"))
  ([filename]
   (let [lines (read-lines filename)
         instructions (parse-input lines)]
     (time (task-01 instructions));
     (time (task-01-as-set instructions));
     (time (task-02 instructions));
     )))

(defn -main [] (day22))