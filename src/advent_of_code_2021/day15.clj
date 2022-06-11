(ns advent-of-code-2021.day15
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string])
  (:gen-class))

(def ^:private inf Double/POSITIVE_INFINITY)

(defn- update-costs
  "Returns costs updated with any shorter paths found to curr's unvisisted
  neighbors by using curr's shortest path"
  [g costs unvisited curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
     (fn [c nbr nbr-cost]
       (if (unvisited nbr)
         (update-in c [nbr] min (+ curr-cost nbr-cost))
         c))
     costs
     (get g curr))))

(defn- dijkstra
  "Returns a map of nodes to minimum cost from src using Dijkstra algorithm.
  Graph is a map of nodes to map of neighboring nodes and associated cost.
  Optionally, specify destination node to return once cost is known"
  ([g src]
   (dijkstra g src nil))
  ([g src dst]
   (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
          curr src
          unvisited (disj (apply hash-set (keys g)) src)]
     (cond
       (= curr dst)
       (select-keys costs [dst])

       (or (empty? unvisited) (= inf (get costs curr)))
       costs

       :else
       (let [next-costs (update-costs g costs unvisited curr)
             next-node (apply min-key next-costs unvisited)]
         (recur next-costs next-node (disj unvisited next-node)))))))


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

(defn- neighbor-coords
  ([coords field] (neighbor-coords (coords 0) (coords 1) field))
  ([r c field]
   (let [coords [[(dec r) c] [r (dec c)] [r (inc c)] [(inc r) c]]]
     (filter #(not (nil? (get-value % field))) coords))))

(defn- neighbors
  ([coords field] (neighbors (coords 0) (coords 1) field))
  ([r c field]
   (map #(get-value % field) (neighbor-coords r c field))))

(defn- ->mat [lines]
  (let [->vec (fn [line] (vec (map #(Integer/parseInt (str %)) line)))]
    (vec (map ->vec lines))))

(defn- ->matbig [lines]
  (let [f (fn [val d] (loop [x (+ val d)]
                        (if (<= x 9) x (recur (- x 9)))))
        ->vec (fn [line] (vec (map #(Integer/parseInt (str %)) line)))
        ->vecbig (fn [line] (vec (mapcat (fn [r] (map #(f % r) (->vec line))) (range 5))))
        firstlines (vec (map ->vecbig lines))
        transform-bigrow (fn [row d] (vec (map #(f % d) row)))
        transform-firstlines (fn [mat d] (vec (map #(transform-bigrow % d) mat)))]
    (vec (mapcat #(transform-firstlines firstlines %) (range 5)))))

(defn- ->graph [mat]
  (let [rows (count mat)
        cols (count (mat 0))]
    (loop [current {[-1 -1] {[0 0] 1}}
           r 0
           c 0]
      (cond
        (>= r rows) current
        :else
        (let [coords [r c]
              neighbor-c (neighbor-coords r c mat)
              neighbor-v (neighbors r c mat)
              new (assoc current coords (zipmap neighbor-c neighbor-v))]
          (cond
            (>= c (dec cols))
            (recur new (inc r) 0)
            :else
            (recur new r (inc c))))))))

(defn- task01 [lines]
  (let [rows (count lines)
        cols (count (lines 0))
        graph (->graph (->mat lines))
        target [(dec rows) (dec cols)]]
    (get (dijkstra graph [0 0] target) target)))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- task02 [lines]
  (let [mat (->matbig lines)
        rows (count mat)
        cols (count (mat 0))
        graph (->graph mat)
        target [(dec rows) (dec cols)]]
    (get (dijkstra graph [0 0] target) target)))

(defn day15
  ([] (day15 "input_day_15.txt"))
  ([filename]
   (let [lines (read-lines filename)]
     (println "Solution Day 15-1:" (task01 lines))
     (println "Solution Day 15-2: 2838 (Precomputed since actually running the program takes 3 hours)")
     ; Don't run this unless you have 3 hours to spare
     ; {[499 499] 2838}
     ; "Elapsed time: 1.05314533394E7 msecs"
     ;(time (println "Solution Day 15-2:" (task02 lines)));
     )))

(defn -main [] (time (day15)))