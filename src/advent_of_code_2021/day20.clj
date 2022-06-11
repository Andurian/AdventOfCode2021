(ns advent-of-code-2021.day20
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string]
            [clojure.set])
  (:gen-class))

(defn ->algorithm [str]
  (vec (map #(if (= \. %) 0 1) str)))

(defn ->image [lines]
  (let [rows (count lines)
        cols (count (first lines))]
    (loop [lit-pixels #{}
           r 0
           c 0]
      (cond
        (>= r rows) {:rows rows :cols cols :pixels lit-pixels :outside \0}
        (>= c cols) (recur lit-pixels (inc r) 0)
        :else (let [new-pixels (if (= \. (nth (nth lines r) c)) lit-pixels (conj lit-pixels [r c]))]
                (recur new-pixels r (inc c)))))))

(defn parse-input [lines]
  (let [algorithm-str (first lines)
        image-lines (drop 2 lines)]
    {:algo (->algorithm algorithm-str)
     :img (->image image-lines)}))

(defn increase-img-size [img]
  (let [new-rows (+ 2 (:rows img))
        new-cols (+ 2 (:cols img))
        new-pixels (set (map #(vector (inc (% 0)) (inc (% 1))) (:pixels img)))
        r0 (if (= \1 (:outside img)) (set (map #(vector 0 %) (range new-cols))) #{})
        rn (if (= \1 (:outside img)) (set (map #(vector (dec new-rows) %) (range new-cols))) #{})
        c0 (if (= \1 (:outside img)) (set (map #(vector % 0) (range new-rows))) #{})
        cn (if (= \1 (:outside img)) (set (map #(vector % (dec new-cols)) (range new-cols))) #{})]
  {:rows (+ 2 (:rows img))
   :cols (+ 2 (:cols img))
   :outside (:outside img)
   :pixels (clojure.set/union new-pixels r0 rn c0 cn)}))

(defn get-pixel [r c img]
  (cond
    (or
     (< r 0)
     (< c 0)
     (>= r (:rows img))
     (>= c (:cols img))) (:outside img)
    :else (if (contains? (:pixels img) [r c]) \1 \0)))

(defn ->region [r c]
  [[(dec r) (dec c)]
   [(dec r) c]
   [(dec r) (inc c)]
   [r (dec c)]
   [r c]
   [r (inc c)]
   [(inc r) (dec c)]
   [(inc r) c]
   [(inc r) (inc c)]])

(defn ->binary [r c img]
  (let [region (->region r c)]
    (clojure.string/join (map #(get-pixel (% 0) (% 1) img) region))))

(defn ->index [r c img]
  (let [string (->binary r c img)]
    (Integer/parseInt string 2)))

(defn img-to-string [img]
  (loop [s [] r 0 c 0]
    (cond
      (>= r (:rows img)) (clojure.string/join s)
      (>= c (:cols img)) (recur (conj s \newline) (inc r) 0)
      :else (let [new-s (conj s (if (contains? (:pixels img) [r c]) \# \.))]
              (recur new-s r (inc c))))))

(defn step
  ([img algo n]
   (loop [current-img img i 0]
     (cond
       (>= i n) current-img
       :else (recur (step current-img algo) (inc i)))))
  ([img algo]
   (let [img-resized (increase-img-size img)
         rows (:rows img-resized)
         cols (:cols img-resized)
         outside (let [algo-point (if (= \0 (:outside img-resized)) (first algo) (last algo))]
                   (if (= 0 algo-point) \0 \1))
         pixels (loop [p #{} r 0 c 0]
                  (cond
                    (>= r rows) p
                    (>= c cols) (recur p (inc r) 0)
                    :else (let [index (->index r c img-resized)
                                new-p (if (= 1 (nth algo index)) (conj p [r c]) p)]
                           ;(println r c "->" index "->" (nth algo index) "-->" new-p)
                            (recur new-p r (inc c)))))]
     {:rows rows
      :cols cols
      :outside outside
      :pixels pixels})))



(defn task01 [lines]
  (let [input (parse-input lines)
        img (:img input)
        algo (:algo input)
        img2 (step img algo 2)
        ]
    (println (img-to-string img2))
    (println "Number of lit pixels:" (count (:pixels img2)))
    ))

(defn task02 [lines]
  (let [input (parse-input lines)
        img (:img input)
        algo (:algo input)
        img2 (step img algo 50)]
    (println (img-to-string img2))
    (println "Number of lit pixels:" (count (:pixels img2)))))

(defn day20
  ([] (println "Using default input")
      (day20 "input_day_20.txt"))
  ([filename]
   (let [lines (read-lines filename)]
     (time (task01 lines));
     (time (task02 lines));
     )))

(defn -main [] (day20))