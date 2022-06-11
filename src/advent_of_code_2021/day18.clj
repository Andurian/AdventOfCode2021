(ns advent-of-code-2021.day18
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string])
  (:gen-class))

(def ^:private l first)
(def ^:private r last)

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- print-path [path]
  (println (vec (map {l \l r \r} path))))

(defn- split-line [line]
  (if (not (= \[ (first line)))
    (clojure.string/split line #"," 2)
    (loop [bracket-count 1
           i 1]
      (if (= 0 bracket-count)
        [(subs line 0 i) (subs line (inc i))]
        (let [curr (nth line i)
              new-count (cond
                          (= \[ curr) (inc bracket-count)
                          (= \] curr) (dec bracket-count)
                          :else bracket-count)]
          (recur new-count (inc i)))))))

(defn- parse-number [line]
  (let [parse-token (fn [token] (if (= \[ (first token))
                                  (parse-number token)
                                  (Integer/parseInt token)))
        content (subs line 1 (dec (count line)))
        tokens (split-line content)]
    [(parse-token (first tokens)) (parse-token (last tokens))]))

(defn- at [path number]
  (if (empty? path) number
      ((apply comp path) number)))

(defn- ->key [path]
  (vec (map {l 0 r 1} (reverse path))))

(defn- left-of [path number]
  (let [stump (drop-while #(= l %) path)]
    (if (empty? stump) nil
        (loop [current-path (cons l (rest stump))]
          (let [current-node (at current-path number)]
            (if (not (vector? current-node)) current-path
                (recur (cons r current-path))))))))

(defn- right-of [path number]
  (let [stump (drop-while #(= r %) path)]
    (if (empty? stump) nil
        (loop [current-path (cons r (rest stump))]
          (let [current-node (at current-path number)]
            (if (not (vector? current-node)) current-path
                (recur (cons l current-path))))))))

(defn- exploding-path
  ([number] (exploding-path number []))
  ([number path]
   (let [current (at path number)]
     (cond
       (>= (count path) 5) (rest path)
       (vector? current)
       (let [result-left-subtree (exploding-path number (cons l path))]
         (if (nil? result-left-subtree)
           (exploding-path number (cons r path))
           result-left-subtree))
       :else nil))))

(defn- split-path
  ([number] (split-path number []))
  ([number path]
   (let [current (at path number)]
     (cond
       (not (vector? current)) (if (>= current 10) path nil)
       :else (let [result-left-subtree (split-path number (cons l path))]
               (if (nil? result-left-subtree)
                 (split-path number (cons r path))
                 result-left-subtree))))))

(defn- explode [number path]
  (let [exploding-pair (at path number)
        a (first exploding-pair)
        b (last exploding-pair)
        path-to-left (left-of path number)
        path-to-right (right-of path number)
        left-of-a (if (nil? path-to-left) nil (at path-to-left number))
        right-of-b (if (nil? path-to-right) nil (at path-to-right number))
        intermediate1 (assoc-in number (->key path) 0)
        intermediate2 (if (nil? path-to-left)
                        intermediate1
                        (assoc-in intermediate1 (->key path-to-left) (+ a left-of-a)))]
    (if (nil? path-to-right)
      intermediate2
      (assoc-in intermediate2 (->key path-to-right) (+ b right-of-b)))))

(defn- split [number path]
  (let [splitting-number (at path number)
        splitted-number [(int (Math/floor (/ splitting-number 2)))
                         (int (Math/ceil (/ splitting-number 2)))]]
    (assoc-in number (->key path) splitted-number)))

(defn- normalize-step [number]
  (let [epath (exploding-path number)]
    (if (nil? epath)
      (let [spath (split-path number)]
        (if (nil? spath)
          number
          (split number spath)))
      (explode number epath))))

(defn- normalize [number]
  (loop [current number
         stepped (normalize-step number)]
    (if (= current stepped)
      current
      (recur stepped (normalize-step stepped)))))

(defn- magnitude [number]
  (if (vector? number)
    (let [a (first number)
          b (last number)
          ma (* 3 (if (vector? a) (magnitude a) a))
          mb (* 2 (if (vector? b) (magnitude b) b))]
      (+ ma mb))
    number))

(defn- add [a b]
  (normalize [a b]))

(defn- add-all [numbers]
  (loop [current (first numbers)
         to-add (rest numbers)]
    (if (empty? to-add)
      current
      (recur (add current (first to-add)) (rest to-add)))))

(defn- max-sum [numbers]
  (loop [max-magnitude 0
         i1 0
         i2 0]
    (if (>= i2 (count numbers))
      max-magnitude
      (if (>= i1 (count numbers))
        (recur max-magnitude 0 (inc i2))
        (let [n1 (nth numbers i1)
              n2 (nth numbers i2)
              s (magnitude (add n1 n2))]
          (recur (max s max-magnitude) (inc i1) i2))))))

(defn- task01 [lines]
  (let [numbers (vec (map parse-number lines))
        result (add-all numbers)]
    (magnitude result)))

(defn- task02 [lines]
  (let [numbers (vec (map parse-number lines))]
    (max-sum numbers)))

(defn day18
  ([] (day18 "input_day_18.txt"))
  ([filename]
   (let [lines (read-lines filename)]
     (println "Solution Day 18-1:" (task01 lines))
     (println "Solution Day 18-2:" (task02 lines)))))

(defn -main [] (time (day18)))