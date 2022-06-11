(ns advent-of-code-2021.day19
  (:require [advent-of-code-2021.util :refer [read-lines]]
            [clojure.string]
            [clojure.set])
  (:gen-class))

(def ^:private overlapping-beacons 12)

(def ^:private orientations [[:x  :y  :z]
                             [:x  :z  :-y]
                             [:x  :-y :-z]
                             [:x  :-z :y]
                             [:-x :-y :z]
                             [:-x :z  :y]
                             [:-x :y  :-z]
                             [:-x :-z :-y]
                             [:y  :z  :x]
                             [:y  :x  :-z]
                             [:y  :-z :-x]
                             [:y  :-x :z]
                             [:-y :-z :x]
                             [:-y :x  :z]
                             [:-y :z  :-x]
                             [:-y :-x :-z]
                             [:z  :x  :y]
                             [:z  :y  :-x]
                             [:z  :-x :-y]
                             [:z  :-y :x]
                             [:-z :-x :y]
                             [:-z :y  :x]
                             [:-z :x  :-y]
                             [:-z :-y :-x]])

(defn- ->coords [line]
  (let [tokens (clojure.string/split line #",")]
    (vec (map #(Integer/parseInt %) tokens))))

(defn- ->scanner [lines]
  (let [id-line (first lines)
        coord-lines (rest lines)
        id (Integer/parseInt (apply str (filter #(Character/isDigit %) id-line)))]
    {:id id
     :detections (vec (map ->coords coord-lines))};
    ))

(defn- ->scanners [lines]
  (let [partitions (filter #(> (count %) 1) (partition-by empty? lines))
        scanners (map ->scanner partitions)]
    (vec scanners)))

(defn- distance [a b]
  (let [dx (- (b 0) (a 0))
        dy (- (b 1) (a 1))
        dz (- (b 2) (a 2))]
    (Math/sqrt
     (+
      (* dx dx)
      (* dy dy)
      (* dz dz)))))

(defn- distance-3d [a b]
  [(- (b 0) (a 0))
   (- (b 1) (a 1))
   (- (b 2) (a 2))])

(defn- distance-manhattan [a b]
  (let [d (distance-3d a b)]
    (apply + (map #(Math/abs %) d))))

(def ^:private axis-transforms {:x #(% 0)
                                :-x #(* -1 (% 0))
                                :y #(% 1)
                                :-y #(* -1 (% 1))
                                :z #(% 2)
                                :-z #(* -1 (% 2))})

(defn- rotate [coord axes]
  (let [transforms (map axis-transforms axes)]
    (vec (map #(% coord) transforms))))

(defn- do-transform [detection transform]
  (let [rotated (rotate detection (:orientation transform))
        r2 (mapv + rotated (:offset transform))]
    r2))

(defn- do-transform-seq [detections transform]
  (vec (map #(do-transform % transform) detections)))

(defn- matches-needed [n]
  (/ (* (dec n) n) 2))

(defn- intra-distances [detections]
  (loop [distances []
         i 0
         j 1]
    (cond
      (>= i (count detections)) distances
      (>= j (count detections)) (recur distances (inc i) (inc (inc i)))
      :else (recur (conj distances (distance (detections i) (detections j))) i (inc j)))))

(defn- matchable? [scanner1 scanner2]
  (let [detections1 (:detections scanner1)
        detections2 (:detections scanner2)
        distances1 (set (intra-distances detections1))
        distances2 (set (intra-distances detections2))
        intersection (clojure.set/intersection distances1 distances2)]
    (if (>= (count intersection) (matches-needed overlapping-beacons)) intersection nil)))

(defn- matchable-to
  ([scanners]
   (loop [current-map {}
          i 0]
     (cond
       (>= i (count scanners)) current-map
       :else (let [matchable-to-i (matchable-to scanners i)]
               (recur (assoc current-map i matchable-to-i) (inc i))))))
  ([scanners i]
   (loop [current-matchable []
          j 0]
     (cond
       (>= j (count scanners)) current-matchable
       (= i j) (recur current-matchable (inc j))
       :else (let [intersections (matchable? (scanners i) (scanners j))
                   new-matchable (if (nil? intersections);
                                   current-matchable;
                                   (conj current-matchable j))]
               (recur new-matchable (inc j)))))))

(defn- inter-set-distances [detections1 detections2]
  (loop [distances [] i 0 j 0]
    (cond
      (>= i (count detections1)) distances
      (>= j (count detections2)) (recur distances (inc i) 0)
      :else (recur (conj distances (distance-3d (detections1 i) (detections2 j))) i (inc j)))))

(defn- correct-rotation [fixed-detections detections axes]
  (let [transform {:orientation axes :offset [0 0 0]}
        rotated-detections (do-transform-seq detections transform)
        distances (inter-set-distances rotated-detections fixed-detections)
        freq (filter (fn [[_ val]] (>= val overlapping-beacons)) (frequencies distances))]
    (if (= 1 (count freq)) ((first freq) 0) nil)))

(defn- find-transform [fixed-detections detections]
  (loop [i 0]
    (cond
      (>= i (count orientations)) nil
      :else (let [dist (correct-rotation fixed-detections detections (orientations i))]
              (if (nil? dist);
                (recur (inc i));
                (let [transform {:orientation (orientations i) :offset dist}
                      transformed-detections (do-transform-seq detections transform)
                      inter (clojure.set/intersection (set fixed-detections) (set transformed-detections))]
                  (when (< (count inter) overlapping-beacons)
                    (throw "finding a transformation failed"))
                  transform))))))

(defn- rotate-seq
  [n coll]
  (let [c (count coll)]
    (take c (drop (mod n c) (cycle coll)))))

(defn- find-unifiable-pair [fixed-ids free-id matchable]
  (let [matchable-ids  (get matchable free-id)
        inter (clojure.set/intersection (set matchable-ids) (set fixed-ids))]
    (if (empty? inter) nil (first inter))))

(defn- unify-scanners [scanners]
  (let [matchables (matchable-to scanners)]
    (loop [fixed-scanners {(:id (first scanners)) (:detections (first scanners))}
           fixed-scanner-positions {(:id (first scanners)) [0 0 0]}
           free-scanners (rest scanners)]
      (cond
        (empty? free-scanners) [fixed-scanners fixed-scanner-positions]
        :else (let [free-scanner (first free-scanners)
                    p (find-unifiable-pair (keys fixed-scanners) (:id free-scanner) matchables)]
                (cond
                  (nil? p) (recur fixed-scanners fixed-scanner-positions (rotate-seq 1 free-scanners))
                  :else (let [fixed-detections (get fixed-scanners p)
                              free-detections (:detections free-scanner)
                              transform (find-transform fixed-detections free-detections)
                              transformed-detections (vec (map #(do-transform % transform) free-detections))
                              new-fixed-scanners (assoc fixed-scanners (:id free-scanner) transformed-detections)
                              new-fixed-scanner-positions (assoc fixed-scanner-positions (:id free-scanner) (:offset transform))]
                          (recur new-fixed-scanners new-fixed-scanner-positions (rest free-scanners)))))))))

(defn- max-distance [positions]
  (loop [max-dist 0
         i 0
         j 1]
    (cond
      (>= i (count positions)) max-dist
      (>= j (count positions)) (recur max-dist (inc i) (inc (inc i)))
      :else (let [new-dist (distance-manhattan (positions i) (positions j))]
              (recur (max max-dist new-dist) i (inc j))))))

(defn day19
  ([] (day19 "input_day_19.txt"))
  ([filename]
   (let [lines (read-lines filename) scanners (->scanners lines)
         res (unify-scanners scanners)
         unified-scanners (res 0)
         unified-offsets (res 1)
         unified-positions (vec (vals unified-offsets))
         unified-detections (apply concat (vals unified-scanners))]
     (println "Solution Day 19-1:" (count (set unified-detections)))
     (println "Solution Day 19-2:" (max-distance (vec unified-positions))))))

(defn -main [] (time (day19)))