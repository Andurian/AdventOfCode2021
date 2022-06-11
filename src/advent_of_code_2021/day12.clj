(ns advent-of-code-2021.day12
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string]
            [clojure.set])
  (:gen-class))

(defn all-uppercase? [s]
  (= s (clojure.string/upper-case s)))

(defn to-adjacency-list [lines]
  (let [pairs (vec (map #(vec (clojure.string/split % #"-")) lines))
        nodes (set (apply concat pairs))
        adjacency-list (into (sorted-map) (map #(vector % (vector)) nodes))]
    (loop [current-list adjacency-list
           remaining-pairs pairs]
      (cond
        (empty? remaining-pairs) current-list
        :else
        (let [connection (first remaining-pairs)
              left-node (connection 0)
              right-node (connection 1)
              reachable-from-left (get current-list left-node)
              reachable-from-right (get current-list right-node)
              reachable-from-left-new (conj reachable-from-left right-node)
              reachable-from-right-new (conj reachable-from-right left-node)]
          (recur (assoc (assoc current-list left-node reachable-from-left-new) right-node reachable-from-right-new) (rest remaining-pairs)))))))

(defn valid-candidate-1 [candidate visited]
  (or (all-uppercase? candidate) (not (contains? (set visited) candidate))))

(defn valid-candidate-2 [candidate visited]
  (let [is-upper (all-uppercase? candidate)
        is-start (= "start" candidate)
        visited-small-caves (filter #(not (all-uppercase? %)) visited)
        can-visit-small-cave (not (contains? (set visited) candidate))
        can-visit-small-cave-twice (= (count visited-small-caves) (count (set visited-small-caves)))]
    (or
     is-upper
     can-visit-small-cave
     (and
      (not is-start)
      can-visit-small-cave-twice))))

(defn all-steps [path connections step-filter]
  (let [current (last path)
        candidates (get connections current)
        valid-candidates (filter #(step-filter % path) candidates)
        new-lists (map #(conj path %) valid-candidates)]
    (if (= current "end") [path] new-lists)))

(defn step-all [paths adjacency-list step-filter]
  (set (mapcat #(vec (all-steps % adjacency-list step-filter)) paths)))

(defn all-paths [connections step-filter]
  (loop [current [["start"]]]
    (if (= (count current) (count (filter #(= (last %) "end") current)))
      current
      (recur (step-all current connections step-filter)))))

(defn print-paths [paths]
  (let [strs (sort (vec (map #(clojure.string/join "," %) paths)))]
    (println (clojure.string/join "\n" strs))))

(defn task01 [lines]
  (let [connections (to-adjacency-list lines)
        paths (all-paths connections valid-candidate-1)]
    (println "Num paths:" (count paths))))

(defn task02 [lines]
  (let [connections (to-adjacency-list lines)
        paths (all-paths connections valid-candidate-2)]
    (println "Num paths:" (count paths))))

(defn day12
  ([] (println "Using default input")
      (day12 "input_day_12.txt"))
  ([filename]
   (let [lines (read-lines filename)]
     (task01 lines);
     (task02 lines);
     )))

(defn -main [] (day12))