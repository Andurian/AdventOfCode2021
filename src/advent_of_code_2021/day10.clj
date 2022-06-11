(ns advent-of-code-2021.day10
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string]
            [clojure.set])
  (:gen-class))

(def matching-paren
  {\( \)
   \[ \]
   \< \>
   \{ \}
   \) \(
   \] \[
   \> \<
   \} \{})

(def scores
  {\)  3
   \]  57
   \}  1197
   \>  25137})

(def completion-scores
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (quot (+ bottom-val top-val) 2)))))

(defn try-pop [stack c]
  (cond
    (= (get matching-paren c) (peek stack)) (pop stack)
    :else (do (println stack "---" c "---" (get matching-paren c))
              (throw (ex-info "" {:violating-char c})))))

(defn process-char [stack c]
  ;(println "Process" c)
  (cond
    (= 1 (count (filter #(= c %) "([{<"))) (vec (conj stack c))
    :else (try-pop stack c)))

(defn line-error-val [line]
  (try
    (loop [stack [] remaining line]
      (cond
        (empty? remaining) 0
        :else (recur (process-char stack (first remaining)) (rest remaining))))
    (catch Exception e
      (let [info (ex-data e)]
        (println "EXCEPTION" info)
        (get scores (get info :violating-char))))))

(defn completion-val [stack]
  (loop [s stack val 0]
    (let [curr (peek s)]
      (cond
        (empty? s) val
        :else (recur (pop s) (+ (get completion-scores (get matching-paren curr)) (* val 5)))))))

(defn line-completion-val [line]
  (try
    (loop [stack [] remaining line]
      (cond
        (empty? remaining) (completion-val stack)
        :else (recur (process-char stack (first remaining)) (rest remaining))))
    (catch Exception _ 0)))

(defn task01 [lines]
  (println (apply + (map line-error-val lines))))

(defn task02 [lines]
  (println (median (filter #(not (= 0 %)) (map line-completion-val lines)))))

(defn day10
  ([] (println "Using default input")
      (day10 "input_day_10.txt"))
  ([filename]
   (let [lines (read-lines filename)]
     (task01 lines);
     (task02 lines);
     )))

(defn -main [] (day10))